///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Akin.hpp"
#include "Convertible.hpp"
#include <stdexcept>


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

   template<class T>
   constexpr auto ToPartialOrdering(T c) noexcept -> ::std::partial_ordering {
      if constexpr (::std::same_as<T, Compared>) {
         switch (c) {
         case Compared::Unordered:
         case Compared::Unknown:
            return ::std::partial_ordering::unordered;
         case Compared::Less:
            return ::std::partial_ordering::less;
         case Compared::Equal:
         case Compared::Equivalent:
            return ::std::partial_ordering::equivalent;
         case Compared::Greater:
            return ::std::partial_ordering::greater;
         }
      }
      else return static_cast<::std::partial_ordering>(c);
   }

   template<class T>
   constexpr auto ToStrongOrdering(T c) -> ::std::strong_ordering {
      if constexpr (::std::same_as<T, Compared>) {
         switch (c) {
         case Compared::Unordered:
         case Compared::Unknown:
            throw ::std::runtime_error {"Partial ordering not mappable to strong ordering"};
         case Compared::Less:
            return ::std::strong_ordering::less;
         case Compared::Equal:
         case Compared::Equivalent:
            return ::std::strong_ordering::equal;
         case Compared::Greater:
            return ::std::strong_ordering::greater;
         }
      }
      else return static_cast<::std::strong_ordering>(c);
   }

   template<class T>
   constexpr auto ToWeakOrdering(T c) -> ::std::weak_ordering {
      if constexpr (::std::same_as<T, Compared>) {
         switch (c) {
         case Compared::Unordered:
         case Compared::Unknown:
            throw ::std::runtime_error {"Partial ordering not mappable to weak ordering"};
         case Compared::Less:
            return ::std::weak_ordering::less;
         case Compared::Equal:
         case Compared::Equivalent:
            return ::std::weak_ordering::equivalent;
         case Compared::Greater:
            return ::std::weak_ordering::greater;
         }
      }
      else return static_cast<::std::weak_ordering>(c);
   }

   constexpr auto FromOrdering(::std::partial_ordering c) noexcept
   -> Compared {
      if      (c == ::std::partial_ordering::unordered)
         return Compared::Unordered;
      else if (c == ::std::partial_ordering::less)
         return Compared::Less;
      else if (c == ::std::partial_ordering::greater)
         return Compared::Greater;
      else
         return Compared::Equivalent;
   }

   constexpr auto FromOrdering(::std::strong_ordering c) noexcept
   -> Compared {
      if (c == ::std::strong_ordering::less)
         return Compared::Less;
      else if (c == ::std::strong_ordering::greater)
         return Compared::Greater;
      else
         return Compared::Equal;
   }

   constexpr auto FromOrdering(::std::weak_ordering c) noexcept
   -> Compared {
      if (c == ::std::weak_ordering::less)
         return Compared::Less;
      else if (c == ::std::weak_ordering::greater)
         return Compared::Greater;
      else
         return Compared::Equivalent;
   }

   constexpr auto FromOrdering(Compared c) noexcept -> Compared {
      return c;
   }

   template<class T1, class T2>
   constexpr auto FromOrdering(const T1& t1, const T2& t2) noexcept
   -> Compared {
      if constexpr (requires { t1 <=> t2; })
         return FromOrdering(t1 <=> t2);
      else if (t1 == t2)
         return Compared::Equal;
      else if (t1 < t2)
         return Compared::Less;
      else
         return Compared::Greater;
   }
}

namespace Langulus::CT
{
   /// Equality comparison check for any LHS and RHS                          
   template<class LHS, class...RHS>
   concept ComparableEqual = PartialValidate<RHS...>
      and requires (const LHS& lhs, const RHS&...rhs) {
         { ((lhs == rhs), ...) } -> Convertible<bool>;
      };

   /// Three-way comparison check for any LHS and RHS                         
   template<class LHS, class...RHS>
   concept Comparable = PartialValidate<RHS...>
      and requires (const LHS& lhs, const RHS&...rhs) {
         { ((lhs <=> rhs), ...) } -> SameAsOneOf<::std::strong_ordering, ::std::partial_ordering, Compared>;
      };

   /// Three-way comparison check for any LHS and RHS.                        
   /// Checks whether the comparison involves strong ordering.                
   /// https://en.cppreference.com/w/cpp/utility/compare/strong_ordering.html 
   template<class LHS, class...RHS>
   concept ComparableStrong = PartialValidate<RHS...>
      and requires (const LHS& lhs, const RHS&...rhs) {
         { ((lhs <=> rhs), ...) } -> ::std::same_as<::std::strong_ordering>;
      };

   /// Three-way comparison check for any LHS and RHS.                        
   /// Checks whether the comparison involves weak ordering.                  
   /// https://en.cppreference.com/w/cpp/utility/compare/weak_ordering.html   
   template<class LHS, class...RHS>
   concept ComparableWeak = PartialValidate<RHS...>
      and requires (const LHS& lhs, const RHS&...rhs) {
         { ((lhs <=> rhs), ...) } -> ::std::same_as<::std::weak_ordering>;
      };

   /// Three-way comparison check for any LHS and RHS.                        
   /// Checks whether the comparison involves partial ordering.               
   /// https://en.cppreference.com/w/cpp/utility/compare/partial_ordering.html
   template<class LHS, class...RHS>
   concept ComparablePartial = PartialValidate<RHS...>
      and requires (const LHS& lhs, const RHS&...rhs) {
         { ((lhs <=> rhs), ...) } -> ::std::same_as<::std::partial_ordering>;
      };
}
