#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Character.hpp>
#include <Langulus/CT/Comparable.hpp>


namespace Langulus::Anyness
{
   
   /// Check if container's elements are comparable                           
   ///   @attention type-erased elements are always insertable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class T1, class...TN>
   concept RangeComparable = CT::Container<C> and (
      C::TypeErased or CT::UnfoldComparable<TypeOf<C>, T1, TN...>
   );

} // namespace Langulus::Anyness

namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements comparison for containers                                   
   ///                                                                        
   struct Comparison {
      using CTTI_Component = Yes;

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using At = typename C::IndexType;

   public:
      template<CT::Container C, CT::Container C2>
      bool operator == (this const C&, const C2&);
      template<CT::Container C, CT::NotContainer T>
      bool operator == (this const C&, const T&) requires RangeComparable<C, T>;

      template<CT::Container C1, CT::Container C2>
      bool Compare(this const C1&, const C2&);
      template<CT::Container C1, CT::Container C2>
      auto Matches(this const C1&, const C2&) noexcept -> Count<C1>;

      template<CT::Container C1, CT::Container C2> requires CT::Character<TypeOf<C1>, TypeOf<C2>>
      bool CompareLoose(this const C1&, const C2&) noexcept;
      template<CT::Container C1, CT::Container C2> requires CT::Character<TypeOf<C1>, TypeOf<C2>>
      auto MatchesLoose(this const C1&, const C2&) noexcept -> Count<C1>;
      
      /// Find a single element's index inside container                      
      ///   @tparam REVERSE - true to perform search in reverse               
      ///   @param item - the item to search for                              
      ///   @param cookie - resume search from a given index                  
      ///   @return the index of the found item, or 'npos' if none found      
      template<bool REVERSE = false, CT::Container C, CT::NoIntent T>
      auto Find(this const C& self, const T& item, Count<C> cookie = 0) noexcept
         -> At<C> requires (C::Indexed and RangeComparable<C, T>)
      {
         if constexpr (not C::TypeErased) {
            auto start = REVERSE ? self.GetRawEnd() - 1 - cookie : self.GetRaw() + cookie;
            auto end   = REVERSE ? start - self.GetCount() : start + self.GetCount();

            while (start != end) {
               if (*start == item)
                  return start - self.GetRaw();

               if constexpr (REVERSE) --start;
               else                   ++start;
            }
         }
         else {
            Count<C> i = REVERSE ? self.GetCount() - 1 - cookie : cookie;
            while (i < self.GetCount()) {
               if (self.GetElementInner(i) == item)
                  return i;

               if constexpr (REVERSE) --i;
               else                   ++i;
            }
         }

         // If this is reached, then no match was found                 
         return Index::None;
      }
   
      /// Find a matching sequence of one or more matching elements           
      ///   @tparam REVERSE - true to perform search in reverse               
      ///   @param range - sequence of items to search for                    
      ///   @param cookie - resume search from a given index                  
      ///   @return the index of the found item, or 'npos' if not found       
      template<bool REVERSE = false, CT::Container C1, CT::Container C2>
      auto FindRange(this const C1& self, const C2& range, Count<C1> cookie = 0) noexcept
         -> At<C1> requires C1::Indexed
      {
         if (cookie >= self.GetCount() or range.GetCount() > self.GetCount() - cookie)
            return Index::None;

         if constexpr (not C1::TypeErased or not C2::TypeErased) {
            // One of the participating blocks is statically typed      
            // Let's check type compatibility first                     
            if constexpr (not C1::TypeErased and not C2::TypeErased) {
               // Leverage the fact, that both participants are typed   
               if constexpr (not CT::Comparable<TypeOf<C1>, TypeOf<C2>>)
                  return Index::None;
            }
            else {
               // One or none of the participants is typed              
               if (not IsSimilar(range))
                  return Index::None;
            }

            // If this is reached reached, then types are comparable    
            auto rhs = range.GetRaw();
            auto lhs = REVERSE ? self.GetRawEnd() - cookie - range.GetCount() : self.GetRaw() + cookie;

            const auto rhsEnd = range.GetRawEnd();
            const auto lhsEnd = REVERSE ? self.GetRaw() - 1 : self.GetRawEnd() - range.GetCount() + 1;

            // This byte size is used ONLY IF both types are binary     
            // compatible. It is simply precomputed here, so that it    
            // isn't recomputed in the loop                             
            [[maybe_unused]] const auto bytesize = self.GetBytesize();

            while (lhs != lhsEnd) {
               if (*lhs == *rhs) {
                  cookie = REVERSE ? self.GetRawEnd() - lhs - 1 : lhs - self.GetRaw();

                  ++lhs;
                  ++rhs;

                  if constexpr (CT::BinaryCompatible<TypeOf<C1>, TypeOf<C2>> and CT::POD<TypeOf<C1>, TypeOf<C2>>) {
                     // We can use batch-compare                        
                     if (0 == memcmp(rhs, lhs, bytesize))
                        return cookie;
                  }
                  else {
                     // Types are not batch-comparable, so compare them 
                     // one by one                                      
                     while (rhs != rhsEnd and *lhs == *rhs) {
                        ++lhs;
                        ++rhs;
                     }

                     if (rhs == rhsEnd)
                        return cookie;
                  }

                  lhs = REVERSE ? self.GetRawEnd() - cookie - 1 : self.GetRaw() + cookie;
                  rhs = range.GetRaw();
               }

               if constexpr (REVERSE) --lhs;
               else                   ++lhs;
            }

            return Index::None;
         }
         else {
            Count<C1> i = REVERSE ? self.GetCount() - 1 - cookie : cookie;
            const auto iend = REVERSE ? static_cast<Count<C1>>(-1) : self.GetCount() - range.GetCount() + 1;

            while (i != iend) {
               if (self.CropInner(i, range.GetCount()) == range)
                  return i;

               if constexpr (REVERSE) --i;
               else                   ++i;
            }

            // If this is reached, then no match was found              
            return Index::None;
         }
      }
   };

} // namespace Langulus::Anyness::Component
