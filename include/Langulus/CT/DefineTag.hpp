///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Literal.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::DefineTag<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_DefineTag = Yes<"TagID">;` in T            
   template<class T>
   struct DefineTag {
      static constexpr Literal Name = "<not a tag>";
      static constexpr bool Enabled = false;
   };

   /// Can be used in two ways to reflect tags:                               
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Tags = <tag or Types<tags...>>;` in T      
   template<class T>
   struct Tags {
      using Type = void;
      static constexpr bool Enabled = false;
   };
}

LANGULUS_CTTI_CONCEPT(DefineTag);

namespace Langulus::RTTI
{
   /// Get the name of a tag definition at compile-time                       
   ///   @tparam T - the tag to get the name of                               
   ///   @return the name                                                     
   template<CT::DefineTag T>
   consteval auto NameOfTag() {
      if constexpr (CTTI::DefineTag<T>::Enabled)
         return CTTI::DefineTag<T>::Name;
      else if constexpr (requires { T::CTTI_DefineTag::Enabled; })
         return T::CTTI_DefineTag::Constant;
      else
         return Literal {""};
   }
}

namespace Langulus::CT::Inner
{
   /// Helper function to extract reflected tags                              
   template<class T>
   consteval auto GetTags() {
      static_assert(not ::std::is_reference_v<T>,
         "Strip references first");
      static_assert(not CT::Convoluted<T>,
         "Strip qualifiers first");

      if constexpr (CTTI::Tags<T>::Enabled) {
         // Checked externally, T doesn't have to be complete           
         return typename CTTI::Tags<T>::Type {};
      }
      else if constexpr (requires { typename T::CTTI_Tags; }) {
         // Checked internally, T has to be a complete type             
         return typename T::CTTI_Tags {};
      }
   };
}

namespace Langulus
{
   /// Get the reflected tags, void if none                                   
   template<class T>
   using TagsOf = decltype(CT::Inner::GetTags<Decvq<Deref<T>>>());
}
