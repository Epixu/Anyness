#pragma once
#include <concepts>


namespace Langulus::CT
{
   
   /// Equality comparable concept for any LHS and RHS, with an adequate      
   /// == operator                                                            
   template<class LHS, class...RHS>
   concept Comparable = requires (const LHS& lhs, const RHS&...rhs) {
         { ((lhs == rhs), ...) } -> ::std::convertible_to<bool>;
      };

} // namespace Langulus::CT