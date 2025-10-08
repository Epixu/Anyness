///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements conversion for containers                                   
   struct Conversion {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using At = typename C::IndexType;

   public:
      //template<CT::Container C, CT::NotContainer TO>
      //bool ConvertTo(this C const&, TO&);

      template<CT::Container C, CT::Container OUT>
      bool ConvertTo(this C const& self, OUT& out) {
         if (self.IsEmpty())
            return false;

         if constexpr (not CT::TypeErased<C> and not CT::TypeErased<OUT>) {
            //                                                          
            // Both containers are statically-typed, so leverage it to  
            // generate a well inlined routine for conversion           
            using TO   = TypeOf<OUT>;
            using FROM = TypeOf<C>;
            
            if constexpr (Same<FROM, TO>) {
               // Types are already the same, just copy elements        
               out.AllocateMore(out.GetCount() + self.GetCount());
               out.Concat(self);
            }
            else if constexpr (CT::Convertible<FROM, TO>) {
               // Types are statically convertible                      
               out.AllocateMore(out.GetCount() + self.GetCount());
               
               for (auto& from : self)
                  out.InsertInner(static_cast<TO>(from));
            }         
         }
         else {
            if (self.IsSame(out)) {
               // Types are already the same, don't convert anything    
               if (out.IsEmpty())
                  out.AssignFrom(self);
               else
                  out.Concat(self);
            }
            
            // Search for a reflected conversion routine                
            LANGULUS_ASSERT(out.GetType(),
               Meta, "Can't convert to unknown type");
            LANGULUS_ASSERT(out.GetType()->mOrigin,
               Meta, "Can't convert to incomplete type `", out.GetType(), '`');

            const auto converter = mType->GetConverter(out.GetType()->mOrigin);
            if (not converter)
               return 0;

            out.template AllocateMore<false, true>(out.mCount + mCount);

            if constexpr (not OUT::TypeErased) {
               if constexpr (CT::Sparse<TO>) {
                  static_assert(CT::Dense<Deptr<TO>>);

                  // We're converting to sparse container                  
                  Block<Decay<TO>> coalesced;
                  coalesced.AllocateFresh(coalesced.RequestSize(mCount));
                  coalesced.mCount = mCount;
                  auto temp = coalesced.GetRaw();
                  auto to = out.GetHandle(0);

                  for (Count i = 0; i < mCount; ++i) {
                     auto from = GetElementDense<CountMax>(i);
                     converter(from.mRaw, temp);
                     to.Assign(temp, coalesced.mEntry);
                     ++to;
                     ++temp;
                  }

                  const_cast<Allocation*>(coalesced.mEntry)
                     ->Keep(mCount - 1);
               }
               else {
                  // We're converting to dense container                   
                  auto to = out.mRaw;
                  for (Count i = 0; i < mCount; ++i) {
                     // Construct each element                             
                     auto from = GetElementDense<CountMax>(i);
                     converter(from.mRaw, to);
                     to += out.GetType()->mSize;
                  }
               }
            }
            else {
               if (out.GetType()->mIsSparse) {
                  if (out.GetType()->mDeptr->mIsSparse)
                     TODO();

                  // We're converting to sparse container                  
                  Block<> coalesced {mType->mOrigin};
                  coalesced.AllocateFresh(coalesced.RequestSize(mCount));
                  coalesced.mCount = mCount;
                  auto temp = coalesced.GetElementInner();
                  auto to = out.template GetHandle<void*>(0);

                  for (Count i = 0; i < mCount; ++i) {
                     auto from = GetElementDense<CountMax>(i);
                     converter(from.mRaw, temp.mRaw);
                     to.Assign(temp.mRaw, coalesced.mEntry);
                     ++to;
                     ++temp;
                  }

                  const_cast<Allocation*>(coalesced.mEntry)
                     ->Keep(mCount - 1);
               }
               else {
                  // We're converting to dense container                   
                  auto to = out.mRaw;
                  for (Count i = 0; i < mCount; ++i) {
                     // Construct each element                             
                     auto from = GetElementDense<CountMax>(i);
                     converter(from.mRaw, to);
                     to += out.GetType()->mSize;
                  }
               }
            }
         }
         return true;
      }
   };
}
