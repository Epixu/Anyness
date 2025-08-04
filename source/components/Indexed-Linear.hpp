///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>
#include <limits>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Provides random element access based on a linear index, that is        
   /// mapped directly onto contiguous memory                                 
   ///   @tparam T - constrain the type of allowed indices. Leave as 'void'   
   ///      to allow for all the usual integer types                          
   template<class T = void>
   struct IndexedLinear {
      using CTTI_Component = Yes<>;
      static constexpr bool Indexed = true;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;
      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>, typename Deref<C>::PickMut, typename Deref<C>::Pick>;
      template<CT::Container C>
      using PickRange = Tif<CT::Mutable<C>, typename Deref<C>::PickRangeMut, typename Deref<C>::PickRange>;
      
      /// Convert an index to an offset                                       
      /// Special indices will be contextualized                              
      /// Unsigned/signed indices are directly forwarded without any overhead 
      ///   @tparam SAFE - whether to throw if index is outside count         
      ///   @param index - the index to simplify                              
      ///   @return the simplified index, as a simple offset                  
      template<bool SAFE, CT::Index INDEX>
      auto SimplifyIndex(const INDEX& index) const has_assumptions -> size_t {
         if constexpr (CT::Same<INDEX, Index>) {
            // This is the most safe path, throws on errors             
            if constexpr (SAFE)
               return Constrain(index).GetOffset();
            else
               return Constrain(index).GetOffsetUnsafe();
         }
         else {
            // Unsafe, works only on assumptions                        
            // Using an integer index explicitly makes a statement,     
            // that you know what you're doing                          
            LglsAssumeUser(not SAFE or index < static_cast<INDEX>(mCount),
               "Integer index out of range"
            );

            if constexpr (CT::Signed<INDEX>) {
               LglsAssumeUser(index >= 0, 
                  "Integer index is below zero, "
                  "use Index for reverse indices instead"
               );
            }

            return index;
         }
      }
      
   public:
      template<CT::Container C>
      auto operator[] (this C&&, CT::Index auto) has_assumptions -> Pick<C> {
         const auto index = SimplifyIndex(idx);
         LANGULUS_ASSERT(index < mCount, Access, "Index out of range");
         if constexpr (TypeErased)
            return GetElement(index);
         else if constexpr (CT::Sparse<TYPE>)
            return LocalRef {GetRaw() + index, mEntry ? GetEntries() + index : nullptr};
         else
            return GetRaw()[index];
      }

      template<CT::Container C>
      auto GetAt(this C&&, CT::Index auto) has_assumptions -> Pick<C>;

      template<CT::Container C>
      auto GetAtDeep(this C&&, CT::Index auto) has_assumptions -> Pick<C>;

      template<CT::NotVoid AS, CT::Container C>
      auto AsAt(this C&&, CT::Index auto) has_assumptions -> Tif<CT::Dense<AS>, AS&, AS>;

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      auto AsAtCast(this C const&, CT::Index auto) -> AS;

      template<CT::Container C>
      auto GetItemAt(this C&&, CT::Index auto) has_assumptions -> Deep<C>;

      template<CT::Container C>
      auto GetItemAtDeep(this C&&, CT::Index auto) has_assumptions-> Deep<C>;

      template<CT::Container C>
      auto GetDeepAt(this C&&, CT::Index auto) has_assumptions-> Deep<C>*;

      template<CT::Container C>
      auto GetIndex(this C const&, CT::Index auto) has_assumptions -> Count<C>;

      template<CT::Container C>
      auto GetIndexMode(this C const&, Count<C>&) has_assumptions -> Count<C>;

      template<CT::Container C>
      auto Last(this C&&) has_assumptions -> Pick<C>;

      template<CT::Container C>
      auto Select(this C&&, CT::Index auto, Count<C>) has_assumptions -> PickRange<C>;

      template<CT::Container C>
      void SwapIndices(this C&, CT::Index auto, CT::Index auto) has_assumptions;
   };
}
