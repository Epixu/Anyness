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
      template<unsigned, bool, bool> friend struct OwnershipEmergent;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      /// Reference all referencable elements inside the container            
      /*template<CT::Container C>
      void KeepDeep(this C const& self) { 
         constexpr bool MASKED = not CT::Contiguous<C>;
         Count<C> remaining = self.GetCount();
         if (not remaining)
            return;

         if constexpr (CT::TypeErased<C>) {
            //                                                          
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

                  referencer(const_cast<uint8_t*>(raw), 1);
                  raw += size;
               }
            }
         }
         else {
            //                                                          
            // Container is statically-typed                            
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

                  (raw++)->Reference(1);
               }
            }
         }
      }*/

      /// Dereference all referenced initialized items, eventually destroying 
      /// them if their individual references reach zero.                     
      ///   @attention never modifies any block state                         
      ///   @attention assumes container has a valid ownership                
      /*template<CT::Container C>
      void FreeDeep(this C& self) {
         constexpr bool MASKED = not CT::Contiguous<C>;
         Count<C> remaining = self.GetCount();
         if (not remaining)
            return;

         LglsAssumeDev(self.GetUses() != 1,
            "You should call DestroyElementDeep instead");
         LglsAssumeDev(self.GetAllocation(),
            "Invalid ownership");

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Container is type-erased                                 
            const auto T = self.GetType();
            const auto referencer = T.GetReferencer();
            const auto destructor = T.GetDestructor();

            if (destructor and referencer) {
               // Destroy every dense element                           
               // Notice that fully dereferenced elements WILL be       
               // destroyed regardless if DESTROY has been requested or 
               // not. This prevents leaks                              
               const auto count = MASKED ? self.GetReserved() : self.GetCount();
               const auto size = T.GetSize();
               auto data = self.template GetRawAs<uint8_t>();
               const auto dataEnd = data + size * count;

               [[maybe_unused]] int index;
               if constexpr (MASKED)
                  index = 0;

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

                  if (not referencer(data, -1))
                     destructor(data);

                  data += size;

                  if constexpr (MASKED)
                     ++index;
               }
            }
         }
         else {
            //                                                          
            // Container is statically-typed                            
            using T = TypeOf<C>;

            if constexpr (CT::Destroyable<T> and CT::Referenced<T>) {
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

                  if constexpr (CT::Referenced<T>) {
                     if (not data->Reference(-1))
                        data->~T();
                  }

                  ++data;
               }
            }
         }
      }*/

      /// Nests through all indirection layers and references elements and    
      /// entries                                                             
      ///   @attention doesn't change any container state                     
      template<CT::Container C>
      void KeepElementDeep(this C& self) has_assumptions {
         static_assert(CT::ContainsOne<C>,
            "Referencing only first element in a container with many");
         if (self.IsEmpty())
            return;

         using H = typename C::HandleMutType;
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Referencing a type-erased element                        
            const auto T = self.GetType();
            if (T.IsSparse()) {
               EntryPtr entries = self.GetEntries();
               if (not entries or not *entries)
                  return;

               const auto subT = T.GetDeptr();
               const auto ptr = *static_cast<void**>(const_cast<void*>(self.GetRaw())); //TODO this won't work for packed pointers
               LglsAssumeDev(ptr, "Null pointer");

               if (subT.IsSparse()) {
                  // Pointer to pointer                                 
                  if (auto subEntry = entries + 1) {
                     H temp {ptr, subEntry, subT};
                     temp.KeepElementDeep();
                  }
               }
               else if (const auto referencer = subT.GetReferencer()) {
                  // Pointer to dense                                   
                  referencer(ptr, 1);
               }

               (*entries)->Keep();
            }
         }
         else {
            //                                                          
            // Referencing a statically-typed element                   
            using T = TypeOf<C>;            
            if constexpr (CT::Sparse<T>) {
               using DT = Deptr<T>;
               EntryPtr entries = self.GetEntries();
               if (not entries or not *entries)
                  return;

               auto& ptr = *self.template GetRawAs<T>();
               LglsAssumeDev(ptr, "Null pointer");

               if constexpr (CT::Sparse<DT>) {
                  // Pointer to pointer                                 
                  using DenserH = typename H::Denser;
                  DenserH temp {ptr, entries + 1};
                  temp.KeepElementDeep();
               }
               else if constexpr (CT::Referenced<DT>) {
                  // Pointer to dense                                   
                  ptr->Reference(1);
               }

               (*entries)->Keep();
            }
         }
      }      

      /// Nests through all indirection layers and destroys elements and      
      /// entries if they're fully dereferenced                               
      ///   @attention doesn't change any container state                     
      template<bool DESTROY = true, CT::Container C>
      void DestroyElementDeep(this C& self) has_assumptions {
         static_assert(CT::ContainsOne<C>,
            "Destroying only first element in a container with many");
         if (self.IsEmpty())
            return;

         using H = typename C::HandleMutType;
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Destroying a type-erased element                         
            const auto T = self.GetType();
            
            if (T.IsSparse()) {
               EntryPtr entries = self.GetEntries();
               if (not entries or not *entries)
                  return;

               // If T is Text**, subT is Text*                         
               const auto subT = T.GetDeptr();
               
               if (1 == (*entries)->GetUses()) {
                  // If T is Text**, ptr becomes Text**                 
                  const auto ptr = *static_cast<void**>(self.GetRaw()); //TODO this won't work for packed pointers
                  LglsAssumeDev(ptr, "Null pointer");

                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     if (auto subEntry = entries + 1) {
                        H temp {ptr, subEntry, subT};
                        temp.template DestroyElementDeep<DESTROY>();
                     }
                  }
                  else if (auto destructor = subT.GetDestructor()) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if (const auto referencer = subT.GetReferencer()) {
                        if (referencer(ptr, -1) == 0)
                           destructor(ptr);
                     }
                     else destructor(ptr);
                  }

                  Allocator::Deallocate(*entries);
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

                  (*entries)->Free();
               }
            }
            else if constexpr (DESTROY) {
               if (const auto destructor = T.GetDestructor()) {
                  // Call destructor of dense element                   
                  const auto ptr = self.GetRaw();
                  IF_SAFE(if (const auto referencer = T.GetReferencer())
                     referencer(ptr, -1));
                  destructor(ptr);
               }
            }
         }
         else {
            //                                                          
            // Destroying a statically-typed element                    
            using T = TypeOf<C>;
            
            if constexpr (CT::Sparse<T>) {
               using DT = Deptr<T>;
               EntryPtr entries = self.GetEntries();
               if (not entries or not *entries)
                  return;

               if (1 == (*entries)->GetUses()) {
                  auto& ptr = *self.template GetRawAs<T>();
                  LglsAssumeDev(ptr, "Null pointer");

                  if constexpr (CT::Sparse<DT>) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     using DenserH = typename H::Denser;
                     DenserH temp{ptr, entries + 1};
                     temp.template DestroyElementDeep<DESTROY>();
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

                  Allocator::Deallocate(*entries);
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

                  (*entries)->Free();
               }
            }
            else if constexpr (DESTROY and CT::Destroyable<T>) {
               // Call destructor of dense element                      
               auto& element = self.Get();
               IF_SAFE(if constexpr (CT::Referenced<T>)
                  element.Reference(-1));
               element.~T();
            }
         }
      }
      
      /// Emplace on top of the first element using an intent                 
      ///   @attention this overwrites previous entries without dereferencing 
      ///   @attention emplacing using a handle is faster due to carrying     
      ///      allocation data with itself when sparse, rather than searching 
      ///      for it on demand.                                              
      ///   @param intent - entries will be copied/sought if handle/sparse    
      template<CT::Container C, CT::Intent I>
      void EmplaceEntries(this C& self, I&& intent) {
         static_assert(not CT::Cloned<I>,
            "EmplaceEntries shouldn't be called when cloning, "
            "because it will overwrite/reference new allocations"
         );
         LglsAssumeDev(self.IsSparse(),
            "EmplaceEntries shouldn't be called on dense containers");
         using IT = Decvq<Deref<TypeOf<I>>>;
         decltype(auto) rhs = FWD(intent.what);

         if constexpr (CT::Handle<IT>)
            LglsAssumeDev(rhs.IsSparse(), "Sparseness mismatch");
         else
            LglsAssumeDev(CT::Sparse<decltype(rhs)>, "Sparseness mismatch");

         const auto indirections = self.GetIndirections();
         const auto entries_size = sizeof(AllocationPtr) * indirections;
         if constexpr ((CT::Handle<IT> and     I::IsKept())
         or (       not CT::Handle<IT> and not CT::Disowned<I>)) {
            // When it's a keeping intent, copy all entries and         
            // reference them                                           
            if constexpr (CT::Handle<IT>)
               memcpy(self.GetEntries(), rhs.GetEntries(), entries_size);
            else
               memset(self.GetEntries(), 0, entries_size);

            if constexpr (CT::Handle<IT> or LANGULUS_FEATURE(MANAGED_MEMORY)) {
               auto entries = self.GetEntries();
               const auto entriesEnd = entries + indirections;
               auto meta = self.GetType().GetDeptr();
               void** handle = self.template GetRawAs<void*>();

               while (entries < entriesEnd) {
                  // When it's a keeping intent, copy all entries and   
                  // reference them. Notice that when NOT emplacing via 
                  // a handle, we're forced to reference on abandon,    
                  // because we can't abandon a raw pointer.            
                  #if LANGULUS_FEATURE(MANAGED_MEMORY)
                     if constexpr (not CT::Handle<IT>)
                        *entries = const_cast<AllocationPtr>(Allocator::Find(meta, *handle));
                  #endif

                  if (not *entries)
                     break;

                  (*entries)->Keep(1);

                  LglsAssumeDev(meta,
                     "Valid entry, but invalid type");
                  LglsAssumeDevAndOptimize(*handle,
                     "Valid entry, but invalid pointer");

                  auto referencer = meta.GetReferencer();
                  if (meta.IsDense() and referencer)
                     referencer(*handle, 1);

                  handle = reinterpret_cast<void**>(*handle);
                  meta = meta.GetDeptr();
                  ++entries;
               }
            }
         }
         else {
            // Disowning just zeroes all entries                        
            memset(self.GetEntries(), 0, entries_size);
         }
      }
   };
}
