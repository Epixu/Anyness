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
   /// Can be used in two ways to satisfy CT::MapsTo<T>:                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_MapsTo = <type or Types<...>>;` in T       
   template<class T>
   struct MapsTo {
      using Type = void;
      static constexpr bool Enabled = false;
   };
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Helper function to extract reflected morphisms                      
      template<class T>
      consteval CT::Typelist auto GetMorphisms() {
         static_assert(not ::std::is_reference_v<T>,
            "Strip references first");

         if constexpr (CTTI::MapsTo<T>::Enabled) {
            // Checked externally, T doesn't have to be complete        
            using LIST = typename CTTI::MapsTo<T>::Type;
            if constexpr (CT::Typelist<LIST>)
               return LIST {};
            else
               return Types<LIST> {};
         }
         else if constexpr (requires { typename T::CTTI_MapsTo; }) {
            // Checked internally, T has to be a complete type          
            using LIST = typename T::CTTI_MapsTo;
            if constexpr (CT::Typelist<LIST>)
               return LIST {};
            else
               return Types<LIST> {};
         }
         else return NoTypes {};
      };
   }

   /// Convertible concept                                                    
   template<class FROM, class...TO>
   concept Convertible = Inner::CheckSize<TO...>()
       and (::std::convertible_to<FROM, TO> and ...);
}

namespace Langulus
{
   /// Get the reflected morphisms, CT::Void if none                          
   template<class T>
   using MorphismsOf = decltype(CT::Inner::GetMorphisms<Decvq<Deref<T>>>());
}
