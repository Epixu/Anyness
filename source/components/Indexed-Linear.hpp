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

   protected:
      template<unsigned, class>
      friend struct Insertion;
      template<unsigned>
      friend struct HeapMovable;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();

      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;

      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>,
         typename Deref<C>::PickMut,
         typename Deref<C>::Pick
      >;

      template<CT::Container C>
      using PickRange = Tif<CT::Mutable<C>,
         typename Deref<C>::PickRangeMut,
         typename Deref<C>::PickRange
      >;
      
      /// Convert an index to an offset                                       
      /// Special indices will be contextualized                              
      /// Unsigned/signed indices are directly forwarded without any overhead 
      ///   @param index - the index to simplify                              
      ///   @return a simple element offset into contiguous memory            
      template<CT::Container C, CT::Index INDEX>
      constexpr auto SimplifyIndex(this C const& self, INDEX index)
      has_assumptions -> Count<C> {
         if constexpr      (::std::same_as<INDEX, Index::Inner::All>)
            static_assert(false, "Index::All can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Many>)
            static_assert(false, "Index::Many can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Single>)
            static_assert(false, "Index::Single can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::None>)
            static_assert(false, "Index::None can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Front>)
            return 0;
         else if constexpr (::std::same_as<INDEX, Index::Inner::Middle>)
            return self.GetCount() / 2;
         else if constexpr (::std::same_as<INDEX, Index::Inner::Back>)
            return self.GetCount();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Mode>)
            return self.GetIndexMode();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Biggest>)
            return self.GetIndexLargest();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Smallest>)
            return self.GetIndexSmallest();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Random>)
            return self.GetIndexRandom();
         else if constexpr (::std::same_as<INDEX, Index::Inner::First>)
            return 0;
         else if constexpr (::std::same_as<INDEX, Index::Inner::Last>) {
            const auto count = self.GetCount();
            return count ? count - 1 : CountMax<C>;
         }
         else if constexpr (requires { index.index; }) {
            const auto c = self.GetCount();
            // If index is negative, wrap it around (if in range)       
            if (index.index < 0)
               return c + index.index >= 0 ? c + index.index : CountMax<C>;
            return index.index >= c ? CountMax<C> : index.index;

         }
         else if constexpr (CT::Integer<INDEX>) {
            // Unsafe, works only on assumptions                        
            // Using an integer index explicitly makes a statement,     
            // that you know what you're doing                          
            LglsAssumeUser(static_cast<Count<C>>(index) < self.GetCount(),
               "Integer index out of range");

            if constexpr (CT::Signed<INDEX>) {
               LglsAssumeUser(index >= 0,
                  "Integer index is below zero, "
                  "use Index::At for reverse indices instead"
               );
            }
            return index;
         }
         else static_assert(false, "Unsupported index type");
      }
      
      /// Select a contiguous region from the memory block - unsafe and may   
      /// return memory that has not been initialized yet                     
      ///   @attention assumes container is typed and allocated               
      ///   @param start - starting element index (included)                  
      ///   @param count - number of sequential elements                      
      ///   @return the selected contiguous range                             
      template<CT::Container C>
      auto SelectInner(this C&& self, Count<C> start, Count<C> count)
      has_assumptions -> PickRange<C> {
         LglsAssumeDev(self.IsAllocated(), "Block is not allocated");
         LglsAssumeDev(self.IsTyped(),     "Block is not typed");
         
         PickRange<C> result {self};
         result.mCount = count;
         result.mHeap += start * result.GetStride();
         return result;
      }

   public:
      /// Subscript operator for accessing element at a specific index        
      ///   @param idx - the index                                            
      ///   @return the picked element                                        
      template<CT::Container C>
      auto operator[] (this C&& self, CT::Index auto idx)
      has_assumptions -> Pick<C> {
         return self.GetAt(idx);
      }

      /// Access element at a specific index                                  
      ///   @param idx - the index                                            
      ///   @return the picked element                                        
      template<CT::Container C>
      auto GetAt(this C&& self, CT::Index auto idx)
      has_assumptions -> Pick<C> {
         const auto offset = self.SimplifyIndex(idx);
         if constexpr (CT::Handle<Pick<C>>)
            return self.GetHandle() += offset;
         else if constexpr (Deref<C>::TypeErased)
            return self.GetRaw() + offset * self.GetStride();
         else
            return *(self.GetRaw() + offset);
      }

      template<CT::NotVoid AS, CT::Container C>
      auto AsAt(this C&&, CT::Index auto) has_assumptions -> Tif<CT::Dense<AS>, AS&, AS>;

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      auto CastAt(this C const&, CT::Index auto) -> AS;

      template<CT::Container C>
      auto GetItemAt(this C&&, CT::Index auto) has_assumptions -> Deep<C>;

      template<CT::Container C>
      auto GetItemAtDeep(this C&&, CT::Index auto) has_assumptions-> Deep<C>;

      template<CT::Container C>
      auto GetDeepAt(this C&&, CT::Index auto) has_assumptions-> Deep<C>*;

      template<CT::Container C>
      auto GetIndexMode(this C const&, Count<C>&) has_assumptions -> Count<C>;

      template<CT::Container C>
      auto Select(this C&&, CT::Index auto, Count<C>) has_assumptions -> PickRange<C>;

      template<CT::Container C>
      void SwapIndices(this C&, CT::Index auto, CT::Index auto) has_assumptions;
   };
}
