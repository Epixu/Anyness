///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../CTTI.hpp"


namespace Langulus
{
   namespace Anyness
   {
      struct Many;
   }

   ///                                                                        
   /// Descriptor intermediate type, used in constructors and assignment      
   /// operators to enable descriptor construction/assignment. The inner type 
   /// is always a reference to a type-erased container.                      
   ///                                                                        
   struct Describe {
   protected:
      using Many = Anyness::Many;
      const Many& mValue;

   public:
      using CTTI_Typed = Many;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract = Yes;
      using CTTI_Unallocatable = Yes;

      Describe() = delete;
      constexpr Describe(const Describe&) noexcept = default;
      explicit constexpr Describe(Describe&&) noexcept = default;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Describe(const Many& value) noexcept
         : mValue {value} {}

      LANGULUS(ALWAYS_INLINED)
      const auto& operator *  () const noexcept { return  mValue; }

      LANGULUS(ALWAYS_INLINED)
      const auto* operator -> () const noexcept { return &mValue; }
   };

} // namespace Langulus

namespace Langulus::CT
{

   /// Check if the T is descriptor-constructible                             
   /// It has to have the T (Describe&&) constructor in order to be so        
   template<class...T>
   concept DescribeConstructible = not Abstract<T...>
       and not Enum<T...> and not Aggregate<T...>
       and requires (const Anyness::Many& a) {
         (T (Describe {a}), ...);
       };
   
   /// Check if the T is descriptor-assignable                                
   /// It has to have the T::operator = (Describe&&) constructor              
   template<class...T>
   concept DescribeAssignable = not Abstract<T...>
       and not Enum<T...> and not Aggregate<T...>
       and requires (T&...lhs, const Anyness::Many& rhs) {
         ((lhs = Describe {rhs}), ...);
       };

} // namespace Langulus::CT
