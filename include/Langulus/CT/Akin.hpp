///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus
{
   namespace Inner
   {
      /// A nested check that strips qualifiers and compares types along      
      /// all levels of indirection                                           
      template<class T1, class T2>
      consteval bool NestedSame() {
         static_assert(not ::std::is_reference_v<T1>, "Strip references first");
         static_assert(not ::std::is_reference_v<T2>, "Strip references first");
         using Stripped1 = Decvq<T1>;
         using Stripped2 = Decvq<T2>;
         if constexpr (::std::same_as<Stripped1, Stripped2>)
            return true;
         else if constexpr (::std::is_pointer_v<Stripped1>
                        and ::std::is_pointer_v<Stripped2>)
            return NestedSame<::std::remove_pointer_t<Stripped1>,
                              ::std::remove_pointer_t<Stripped2>>();
         else return false;
      }
   }

   /// True if decayed T1 matches all decayed TN types                        
   ///   @attention ignores type density, references, and cv-qualifications   
   template<class T1, class...TN>
   concept Akin = CT::PartialValidate<TN...>
       and (::std::same_as<Decay<T1>, Decay<TN>> and ...);

   /// True if unqualified T1 matches all unqualified TN types                
   ///   @attention ignores cv-qualifications and references only             
   template<class T1, class...TN>
   concept Same = CT::PartialValidate<TN...>
       and (Inner::NestedSame<Deref<T1>, Deref<TN>>() and ...);

   /// True if T1 matches exactly all the provided TN, including              
   /// density and cv-qualifiers. References are ignored here as well.        
   /// Just use std::same_as if you want to match those as well.              
   template<class T1, class...TN>
   concept Exact = CT::PartialValidate<TN...>
       and (::std::same_as<Deref<T1>, Deref<TN>> and ...);

   /// True if decayed T1 matches at least one of the decayed TN              
   ///   @attention ignores type density, references, and cv-qualifications   
   template<class T1, class...TN>
   concept AkinAsOneOf = CT::PartialValidate<TN...>
       and (::std::same_as<Decay<T1>, Decay<TN>> or ...);

   /// True if unqualified T1 matches at least one of the unqualified TN      
   ///   @attention ignores cv-qualifications and references only             
   template<class T1, class...TN>
   concept SameAsOneOf = CT::PartialValidate<TN...>
       and (Inner::NestedSame<Deref<T1>, Deref<TN>>() or ...);

   /// True if T1 matches exactly at least one of the TN, including           
   /// density and cv-qualifications. References are ignored.                 
   template<class T1, class...TN>
   concept ExactAsOneOf = CT::PartialValidate<TN...>
       and (::std::same_as<Deref<T1>, Deref<TN>> or ...);
}
