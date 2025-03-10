///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <concepts>


namespace Langulus::CTTI
{
   
   namespace Inner
   {

      /// Test if T is initializable with a signed fundamental                
      /// std::is_signed_v is crap, because it assumes that all types are     
      /// int-initializable. This one is better, because it allows tests for  
      /// float- and double-initializables as well                            
      template<class T, CT::Fundamental F>
      consteval bool Signed() {
         return ::std::constructible_from<T, F> and requires {
            T {F {-1}} < T {F {0}};
         };
      }

   } // namespace Langulus::CT::Inner

   /// Can be used in two ways to satisfy CT::Signed<T>:                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Signed = Yes;` in T                        
   template<class T>
   struct Signed {
      static constexpr bool Enabled = Inner::Signed<T, int>()
                                   or Inner::Signed<T, float>()
                                   or Inner::Signed<T, double>();
   };
   
} // namespace Langulus::CTTI

namespace Langulus::CT
{

   /// Check if all T are signed                                              
   ///   @attention doesn't apply to numbers only, but anything negatable     
   template<class...T>
   concept Signed = ((CTTI::Signed<Shed<T>>::Enabled or Shed<T>::CTTI_Signed::Enabled) and ...);

   /// Check if all T are unsigned                                            
   ///   @attention doesn't apply to numbers only, but anything negatable     
   template<class...T>
   concept Unsigned = ((not Signed<T>) and ...);

} // namespace Langulus::CT
