///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "../Allocator.hpp"
#include <Langulus/Assume.hpp>
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Referenced.hpp>
#include <Langulus/CT/Contiguous.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Manages deep ownership by searching for an allocation every time       
   ///   @tparam ID - which heap/stack are we keeping track of?               
   template<unsigned ID>
   struct OwnershipDeepEmergent {
      using CTTI_Component = Yes<>;
      
      static constexpr bool DeeplyOwned = true;
      static constexpr int  ComponentPrecedence = 2000;

   protected:
      template<unsigned> friend struct HeapMovable;
      template<unsigned> friend struct Removal;
      template<unsigned> friend struct Emplacement;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      /// Reference referencable elements inside the block                    
      template<CT::Container C>
      void KeepDeep(this C const& self) { 
         constexpr bool MASKED = not CT::Contiguous<C>;
         Count<C> remaining = self.GetCount();
         if (not remaining)
            return;

         if constexpr (not C::TypeErased) {
            // Container is statically typed                            
            using T = TypeOf<C>;

            if constexpr (CT::Referenced<T>) {
               const auto count = MASKED ? self.GetReserved() : self.GetCount();
               const auto rawBeg = self.GetRaw();
               auto raw = rawBeg;
               const auto rawEnd = raw + count;

               while (raw != rawEnd) {
                  if constexpr (MASKED) {
                     if (not remaining)
                        break;

                     if (not self.mTable[raw - rawBeg]) {
                        ++raw;
                        continue;
                     }

                     --remaining;
                  }

                  DecvqCast(raw++)->Reference(1);
               }
            }
         }
         else {
            // Container is type-erased                                 
            const auto T = self.GetType();
            const auto referencer = T.GetReferencer();

            if (referencer) {
               const auto count = MASKED ? self.GetReserved() : self.GetCount();
               const auto size = T.GetSize();
               const auto rawBeg = self.template GetRawAs<uint8_t>();
               auto raw = rawBeg;
               const auto rawEnd = raw + size * count;

               while (raw != rawEnd) {
                  if constexpr (MASKED) {
                     if (not remaining)
                        break;

                     if (not self.mTable[raw - rawBeg]) {
                        raw += size;
                        continue;
                     }

                     --remaining;
                  }

                  referencer(raw, 1);
                  raw += size;
               }
            }
         }
      }

      /// Dereference all referenced initialized items, eventually destroying 
      /// them if their individual references reach zero.                     
      ///   @attention never modifies any block state                         
      ///   @attention assumes block is not empty                             
      ///   @attention assumes block is not static                            
      ///   @tparam DESTROY - used only when GetUses() == 1                   
      template<bool DESTROY = true, CT::Container C>
      void FreeDeep(this C& self) {
         constexpr bool MASKED = not CT::Contiguous<C>;
         Count<C> remaining = self.GetCount();
         if (not remaining)
            return;

         LglsAssumeDev(not DESTROY or self.GetUses() == 1,
            "Attempting to destroy elements used from multiple locations");
         LglsAssumeDev(self.GetAllocation(),
            "Destroying elements in a static container is not allowed");

         if constexpr (not C::TypeErased) {
            // Container is statically typed                            
            using T = TypeOf<C>;

            if constexpr (CT::Destroyable<T> and (DESTROY or CT::Referenced<T>)) {
               const auto count = MASKED ? self.GetReserved() : self.GetCount();
               auto data = self.GetRaw();
               const auto dataEnd = data + count;
               const auto begMarker = data;

               while (data != dataEnd) {
                  if constexpr (MASKED) {
                     if (not remaining)
                        break;

                     if (not self.mTable[data - begMarker]) {
                        ++data;
                        continue;
                     }

                     --remaining;
                  }

                  if constexpr (DESTROY) {
                     if constexpr (CT::Referenced<T>)
                        data->Reference(-1);
                     data->~T();
                  }
                  else if constexpr (CT::Referenced<T>) {
                     if (not data->Reference(-1))
                        data->~T();
                  }

                  ++data;
               }
            }
         }
         else {
            // Container is type-erased                                 
            const auto T = self.GetType();
            const auto referencer = T.GetReferencer();
            const auto destructor = T.GetDestructor();

            if (destructor and (DESTROY or referencer)) {
               // Destroy every dense element                           
               // Notice that fully dereferenced elements WILL be       
               // destroyed regardless if DESTROY has been requested or 
               // not. This prevents leaks                              
               const auto count = MASKED ? self.GetReserved() : self.GetCount();
               const auto size = T.GetSize();
               const auto data = self.template GetRawAs<uint8_t>();
               const auto dataEnd = data + size * count;

               [[maybe_unused]] int index;
               if constexpr (MASKED)
                  index = 0;

               if (referencer) {
                  while (data != dataEnd) {
                     if constexpr (MASKED) {
                        if (not remaining)
                           break;

                        if (not self.mTable[index]) {
                           data += size;
                           ++index;
                           continue;
                        }

                        --remaining;
                     }

                     if constexpr (DESTROY) {
                        referencer(data, -1);
                        destructor(data);
                     }
                     else if (not referencer(data, -1))
                        destructor(data);

                     data += size;

                     if constexpr (MASKED)
                        ++index;
                  }
               }
               else if constexpr (DESTROY) {
                  while (data != dataEnd) {
                     if constexpr (MASKED) {
                        if (not remaining)
                           break;

                        if (not self.mTable[index]) {
                           data += size;
                           ++index;
                           continue;
                        }

                        --remaining;
                     }

                     destructor(data);
                     data += size;

                     if constexpr (MASKED)
                        ++index;
                  }
               }
            }
         }

         // Always nullify upon destruction only if we're paranoid         
         //TODO IF_LANGULUS_PARANOID(ZeroMemory(mRaw, GetBytesize<THIS>()));
      }

      /// Nests through all indirection layers and destroys                   
      /// elements and entries if they're fully dereferenced                  
      ///   @attention doesn't change any container state                     
      template<CT::Container C>
      void DestroyElement(this C& self) has_assumptions {
         static_assert(CT::ContainsOne<C>);

         if constexpr (CT::TypeErased<C>) {
            // Destroying a type-erased element                         
            const auto T = self.GetType();
            
            if (T.IsSparse()) {
               auto& entry = self.GetEntry();
               if (not entry)
                  return;
               const auto subT = T.GetDeptr();
               
               if (1 == entry->GetUses()) {
                  const auto ptr = *static_cast<void**>(self.GetRaw()); //TODO this won't work for packed pointers
                  LglsAssumeDev(ptr, "Null pointer");

                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     if (auto subEntry = Allocator::Find(subT, ptr)) //TODO extract entry from previous entry?
                        C {ptr, subEntry, subT}.DestroyElement();
                  }
                  else if (subT.GetDestructor()) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if (const auto referencer = subT.GetReferencer()) {
                        if (referencer(ptr, -1) == 0)
                           subT.GetDestructor()(ptr);
                     }
                     else subT.GetDestructor()(ptr);
                  }

                  Allocator::Deallocate(entry);
               }
               else {
                  // This element occurs in more than one place.        
                  // We're not allowed to deallocate the memory behind  
                  // it, but we must call destructors if T is           
                  // referencable and its individual references have    
                  // reached 0. This can happen when hive elements are  
                  // dereferenced.                                      
                  const auto referencer = subT.GetReferencer();
                  if (not subT.IsSparse() and referencer) {
                     const auto ptr = *static_cast<void**>(self.GetRaw()); //TODO this won't work for packed pointers
                     if (referencer(ptr, -1) == 0)
                        subT.GetDestructor()(ptr);
                  }

                  entry->Free();
               }
            }
            else {
               if (const auto destructor = T.GetDestructor()) {
                  // Call destructor of dense element                   
                  const auto ptr = self.GetRaw();
                  if (const auto referencer = T.GetReferencer())
                     referencer(ptr, -1);
                  destructor(ptr);
               }
            }
         }
         else {
            // Destroying a statically-typed element                    
            using T = TypeOf<C>;
            
            if constexpr (CT::Sparse<T>) {
               using DT = Deptr<T>;
               auto& entry = self.GetEntry();
               if (not entry)
                  return;

               if (1 == entry->GetUses()) {
                  auto& ptr = *self.template GetRawAs<T>();
                  LglsAssumeDev(ptr, "Null pointer");

                  if constexpr (CT::Sparse<DT>) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     C {ptr}.DestroyElement();
                  }
                  else if constexpr (CT::Destroyable<DT>) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if constexpr (CT::Referenced<DT>) {
                        if (ptr->Reference(-1) == 0)
                           ptr->~DT();
                     }
                     else ptr->~DT();
                  }

                  Allocator::Deallocate(entry);
               }
               else {
                  // This element occurs in more than one place.        
                  // We're not allowed to deallocate the memory behind  
                  // it, but we must call destructors if T is           
                  // referencable and its individual references have    
                  // reached 0. This can happen when hive elements are  
                  // dereferenced.                                      
                  if constexpr (CT::Dense<DT> and CT::Referenced<DT>) {
                     auto& ptr = *self.template GetRawAs<T>();
                     if (ptr->Reference(-1) == 0)
                        ptr->~DT();
                  }

                  entry->Free();
               }
            }
            else if constexpr (CT::Destroyable<T>) {
               // Call destructor of dense element                      
               auto& element = self.Get();
               if constexpr (CT::Referenced<T>)
                  element.Reference(-1);
               element.~T();
            }
         }
      }
   };
}
