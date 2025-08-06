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
   ///                                                                        
   template<unsigned ID = 0>
   struct DeepOwnershipStack {
      using CTTI_Component = Yes<>;
      static constexpr bool DeeplyOwned = true;

   protected:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      // Pointer to the first entry                                     
      AllocationPtr* mEntries = nullptr;

      /// Get entries array                                                   
      auto GetEntries() const noexcept { return mEntries; }

      /// Reference referencable elements inside the block                    
      template<CT::Container C>
      void KeepDeep(this C const& self) { 
         constexpr bool MASKED = not CT::IndexedLinearly<C>;
         [[maybe_unused]] Count<C> remaining;
         if constexpr (MASKED)
            remaining = self.GetCount();
         const auto count = MASKED ? self.GetReserved() : self.GetCount();

         if constexpr (not C::TypeErased) {
            using T = TypeOf<C>;
            if constexpr (CT::Sparse<T> and CT::Referenced<Deptr<T>>) {
               // Statically typed and sparse                           
               const auto entryBeg = GetEntries();
               auto entry = entryBeg;
               const auto entryEnd = entry + count;

               while (entry != entryEnd) {
                  if constexpr (MASKED) {
                     if (not remaining)
                        break;

                     if (not mask[entry - entryBeg]) {
                        ++entry;
                        continue;
                     }

                     --remaining;
                  }

                  if (*entry) {
                     const_cast<Allocation*>(*entry)->Keep();
                     DecvqCast(GetRaw()[entry - GetEntries()])->Reference(1);
                  }

                  ++entry;
               }
            }
            else if constexpr (CT::Referenced<T>) {
               // Statically typed and dense                            
               const auto rawBeg = GetRaw();
               auto raw = rawBeg;
               const auto rawEnd = raw + count;

               while (raw != rawEnd) {
                  if constexpr (MASKED) {
                     if (not remaining)
                        break;

                     if (not mask[raw - rawBeg]) {
                        ++raw;
                        continue;
                     }

                     --remaining;
                  }

                  DecvqCast(raw++)->Reference(1);
               }
            }
         }
         else if (mType->mIsSparse and mType->mReference) {
            // Type-erased and sparse                                   
            const auto reference = mType->mReference;
            const auto entryBeg = GetEntries();
            auto entry = entryBeg;
            const auto entryEnd = entry + count;

            while (entry != entryEnd) {
               if constexpr (MASKED) {
                  if (not remaining)
                     break;

                  if (not mask[entry - entryBeg]) {
                     ++entry;
                     continue;
                  }

                  --remaining;
               }

               if (*entry) {
                  const_cast<Allocation*>(*entry)->Keep();
                  reference(mRawSparse[entry - GetEntries()], 1);
               }

               ++entry;
            }
         }
         else if (mType->mReference) {
            // Type-erased and dense                                    
            const auto reference = mType->mReference;
            const auto rawBeg = mRaw;
            auto raw = rawBeg;
            const auto rawEnd = mRaw + mType->mSize * count;

            while (raw != rawEnd) {
               if constexpr (MASKED) {
                  if (not remaining)
                     break;

                  if (not mask[raw - rawBeg]) {
                     raw += mType->mSize;
                     continue;
                  }

                  --remaining;
               }

               reference(raw, 1);
               raw += mType->mSize;
            }
         }
      }

   public:
      constexpr DeepOwnershipStack() noexcept = default;
      constexpr DeepOwnershipStack(DeepOwnershipStack const&) noexcept = default;
      constexpr DeepOwnershipStack(DeepOwnershipStack&&) noexcept = default;
      constexpr DeepOwnershipStack(AllocationPtr* entries) noexcept
         : mEntries {entries} {}
   };
}
