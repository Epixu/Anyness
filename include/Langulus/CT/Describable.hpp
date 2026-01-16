///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Abstract.hpp"


namespace Langulus
{
   namespace Anyness
   {
      struct Many;
   }

   ///                                                                        
   /// Descriptor intermediate type, used in constructors and assignment      
   /// operators to enable describe-construction/assignment. The inner type   
   /// is always a reference to a type-erased container.                      
   /// You should #include <Langulus/Anyness/Many.hpp>                        
   ///        and #include <Langulus/CT/Describable.hpp>                      
   ///        in order to use Describe semantics                              
   struct Describe {
      using Many = Anyness::Many;
      const Many& what;

      //using CTTI_Typed         = Many;
      using CTTI_ReflectAs     = void;
      using CTTI_Abstract      = Yes<>;
      using CTTI_Allocatable   = No;
      using CTTI_Intent        = Yes<>;

      Describe() = delete;
      constexpr Describe(const Describe&) noexcept = default;
      explicit constexpr Describe(Describe&&) noexcept = default;

      explicit constexpr Describe(const Many& descriptor) noexcept
         : what {descriptor} {}

      auto& operator *  () const noexcept { return  what; }
      auto* operator -> () const noexcept { return &what; }
   };
}

namespace Langulus::CT
{
   /// Check if all T are describe-constructible.                             
   /// It has to have the T (Describe&&) constructor in order to be so.       
   template<class...T>
   concept DescribeConstructible = not Abstract<T...>
       and not Enum<T...> and not Aggregate<T...>
       and requires (const Anyness::Many& a) {
         (T (Describe {a}), ...);
       };
   
   /// Check if all T are describe-assignable.                                
   /// It has to have the T::operator = (Describe&&) constructor.             
   template<class...T>
   concept DescribeAssignable = not Abstract<T...>
       and not Enum<T...> and not Aggregate<T...>
       and requires (T&...lhs, const Anyness::Many& rhs) {
         ((lhs = Describe {rhs}), ...);
       };
}
