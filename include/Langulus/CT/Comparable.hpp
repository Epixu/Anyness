#pragma once
#include <concepts>
#include <compare>


namespace Langulus::CT
{
   /// Three-way comparison check for any LHS and RHS                         
   template<class LHS, class...RHS>
   concept Comparable = requires (const LHS& lhs, const RHS&...rhs) {
      { ((lhs <=> rhs), ...) } -> ::std::convertible_to<::std::partial_ordering>;
   };

   /// Three-way comparison check for any LHS and RHS                         
   /// Checks whether the comparison involves strong ordering                 
   /// https://en.cppreference.com/w/cpp/utility/compare/strong_ordering.html 
   template<class LHS, class...RHS>
   concept ComparableStrong = requires (const LHS& lhs, const RHS&...rhs) {
      { ((lhs <=> rhs), ...) } -> ::std::same_as<::std::strong_ordering>;
   };

   /// Three-way comparison check for any LHS and RHS                         
   /// Checks whether the comparison involves weak ordering                   
   /// https://en.cppreference.com/w/cpp/utility/compare/weak_ordering.html   
   template<class LHS, class...RHS>
   concept ComparableWeak = requires (const LHS& lhs, const RHS&...rhs) {
      { ((lhs <=> rhs), ...) } -> ::std::same_as<::std::weak_ordering>;
   };

   /// Three-way comparison check for any LHS and RHS                         
   /// Checks whether the comparison involves partial ordering                
   /// https://en.cppreference.com/w/cpp/utility/compare/partial_ordering.html
   template<class LHS, class...RHS>
   concept ComparablePartial = requires (const LHS& lhs, const RHS&...rhs) {
      { ((lhs <=> rhs), ...) } -> ::std::same_as<::std::partial_ordering>;
   };
}

namespace Langulus
{
   /// Unified comparison results                                             
   enum class Compared {
      Unordered = -128, // Can't be compared                            
      Unknown = -127,   // Not compared yet                             
      Less = -1,        // LHS <  RHS (strong/weak/partial ordering)    
      Equal = 0,        // LHS == RHS (strong ordering)                 
      Greater = 1,      // LHS >  RHS (strong/weak/partial ordering)    
      Equivalent = 2    // LHS == RHS (weak/partial ordering)           
   };
}
