#pragma once
#include "../Values.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Constants<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Values = Values<constants...>;` in T       
   template<class T>
   struct Constants {
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

      if constexpr (CTTI::Constants<T>::Enabled) {
         // Checked externally, T doesn't have to be complete           
         return typename CTTI::Constants<T>::Type {};
      }
      else if constexpr (requires { typename T::CTTI_Values; }) {
         // Checked internally, T has to be a complete type             
         return typename T::CTTI_Values {};
      }
      else return Values<> {};
   };
}

namespace Langulus
{
   /// Get the reflected named values, CT::Void if none                       
   template<class T>
   using NamedValuesOf = decltype(CT::Inner::GetNamedValues<Decvq<Deref<T>>>());
}
