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


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Manages deep ownership by holding a pointer to the entries locally     
   ///   @tparam ID - which heap are we keeping track of?                     
   template<unsigned ID>
   struct DeepOwnershipStack {
      using CTTI_Component = Yes<>;
      using StackRequest = AllocationPtr*;
      
      static constexpr bool DeeplyOwned = true;
      static constexpr int  ComponentPrecedence = 2000;

   protected:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      /// Get entries array (inner)                                           
      constexpr auto& GetEntriesInner(this auto const& self) noexcept {
         return *reinterpret_cast<AllocationPtr* const*>(
            self.mStack + self.template StackOffset<DeepOwnershipStack>
         );
      }
      
      /// Set the entries array ppinter (inner)                               
      constexpr void SetEntriesInner(this auto& self, AllocationPtr* e) noexcept {
         const_cast<AllocationPtr*&>(self.GetEntriesInner()) = e;
      }

      /// Reference referencable elements inside the block                    
      template<CT::Container C>
      void KeepDeep(this C const& self) { 
         constexpr bool MASKED = not CT::IndexedLinearly<C>;
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

      /// Dereference all referenced initialized items, optionally destroying 
      /// them if references reach zero                                       
      ///   @attention never modifies any block state                         
      ///   @attention assumes block is not empty                             
      ///   @attention assumes block is not static                            
      ///   @tparam DESTROY - used only when GetUses() == 1                   
      template<bool DESTROY = true, CT::Container C>
      void FreeDeep(this C& self) {
         constexpr bool MASKED = not CT::IndexedLinearly<C>;
         Count<C> remaining = self.GetCount();
         if (not remaining)
            return;

         LglsAssumeDev(not DESTROY or self.GetUses() == 1,
            "Attempting to destroy elements used from multiple locations");
         LglsAssumeDev(not self.IsStatic(),
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
   };
}
