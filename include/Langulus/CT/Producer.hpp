///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Producer<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Producer = <Factory Type>;` in T           
   template<class T>
   struct Producer {
      using Type = void;
      static constexpr bool Enabled = false;
   };
}

namespace Langulus::CT::Inner
{
   /// Helper function to extract producer type                               
   template<class T>
   consteval CT::Typelist auto GetProducerType() {
      static_assert(not ::std::is_reference_v<T>,
         "Strip references first");
      static_assert(not CT::Convoluted<T>,
         "Strip constness/volatility first");

      if constexpr (NotVoid<typename CTTI::Producer<T>::Type>) {
         // Checked externally, T doesn't have to be complete           
         return Types<typename CTTI::Producer<T>::Type> {};
      }
      else if constexpr (requires { typename T::CTTI_Producer; }) {
         // Checked internally, T has to be a complete type             
         return Types<typename T::CTTI_Producer> {};
      }
      else return NoTypes {};
   };
}

namespace Langulus
{
   /// Get the reflected producer type, void if none                          
   template<class T>
   using ProducerOf = typename decltype(CT::Inner::GetProducerType<Decvq<Deref<T>>>())::First;

   namespace CT
   {
      /// Check if all T has valid producer                                   
      ///   @attention the producer type must not be 'void', in order for T   
      ///      to be considered 'producible'                                  
      template<class...T>
      concept Producible = CT::Inner::CheckSize<T...>()
          and (NotVoid<ProducerOf<Decvq<Deref<T>>>> and ...);

      /// Check if all T have no producers                                    
      template<class...T>
      concept Unproducible = CT::Inner::CheckSize<T...>()
          and ((not Producible<Decvq<Deref<T>>>) and ...);
   }
}
