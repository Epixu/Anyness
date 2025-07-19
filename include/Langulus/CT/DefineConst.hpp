///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Values.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to reflect named values:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Values = Values<constants...>;` in T       
   template<class T>
   struct DefineConstant {
      using Type = void;
      static constexpr bool Enabled = false;
   };
}

namespace Langulus::CT::Inner
{
   /// Helper function to extract reflected named values                      
   template<class T>
   consteval auto GetNamedValues() {
      static_assert(not ::std::is_reference_v<T>,
         "Strip references first");
      static_assert(not CT::Convoluted<T>,
         "Strip qualifiers first");

      if constexpr (CTTI::DefineConstant<T>::Enabled) {
         // Checked externally, T doesn't have to be complete           
         return typename CTTI::DefineConstant<T>::Type {};
      }
      else if constexpr (requires { typename T::CTTI_Values; }) {
         // Checked internally, T has to be a complete type             
         return typename T::CTTI_Values {};
      }
   };
}

namespace Langulus
{
   /// Get the reflected named values, void if none                           
   template<class T>
   using NamedValuesOf = decltype(CT::Inner::GetNamedValues<Decvq<Deref<T>>>());
}
