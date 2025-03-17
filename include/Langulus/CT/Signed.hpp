///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../CTTI.hpp"


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

LANGULUS_CTTI_CONCEPT(Signed);

namespace Langulus::CT
{

   template<class...T>
   concept Unsigned = NotSigned<T>..>;

} // namespace Langulus::CT
