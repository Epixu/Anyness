///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Abstract.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Concrete<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Concrete = <concrete type>;` in T          
   template<class T>
   struct Concrete {
      using Type = void;
      static constexpr bool Enabled = false;
   };
}

namespace Langulus::CT::Inner
{
   /// Helper function to extract concrete type                               
   template<class T>
   consteval CT::Typelist auto GetConcreteType() {
      static_assert(not ::std::is_reference_v<T>,
         "Strip references first");
      static_assert(not CT::Convoluted<T>,
         "Strip constness/volatility first");

      if constexpr (not CT::Abstract<T>) {
         // T has to be abstract in order to be concretizable           
         return NoTypes {};
      }
      else if constexpr (CTTI::Concrete<T>::Enabled) {
         // Checked externally, T doesn't have to be complete           
         return Types<typename CTTI::Concrete<T>::Type> {};
      }
      else if constexpr (requires { typename T::CTTI_Concrete; }) {
         // Checked internally, T has to be a complete type             
         return Types<typename T::CTTI_Concrete> {};
      }
      else return NoTypes {};
   };
}

namespace Langulus
{
   /// Get the reflected concrete type, void if none                          
   template<class T>
   using ConcreteOf = typename decltype(CT::Inner::GetConcreteType<Decvq<Deref<T>>>())::First;

   namespace CT
   {
      /// Check if all T has valid concretization                             
      ///   @attention the concrete type must not be 'void', in order for T   
      ///      to be considered 'concretizable'                               
      template<class...T>
      concept Concretizable = Inner::CheckSize<T...>()
          and (NotVoid<ConcreteOf<Decvq<Deref<T>>>> and ...);

      /// Check if all T have no concretizations                              
      template<class...T>
      concept Unconcretizable = Inner::CheckSize<T...>()
          and ((not Concretizable<Decvq<Deref<T>>>) and ...);
   }
}
