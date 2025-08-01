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
   struct DefineTag;

   /// Can be used in two ways to reflect tags:                               
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Tags = <tag or Types<tags...>>;` in T      
   template<class T>
   struct Tags;
}

LANGULUS_CTTI_CONCEPT(DefineTag);

namespace Langulus::RTTI
{
   /// Get the name of a tag definition at compile-time                       
   ///   @tparam T - the tag to get the name of                               
   ///   @return a compile-time string                                        
   template<CT::DefineTag T>
   consteval auto NameOfTag() {
      if constexpr (CT::Complete<CTTI::DefineTag<T>>)
         return CTTI::DefineTag<T>::Name;
      else
         return T::CTTI_DefineTag::Constant;
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

      if constexpr (CT::Complete<CTTI::Tags<T>>) {
         // Checked externally, T doesn't have to be complete           
         return typename CTTI::Tags<T>::Type {};
      }
      else if constexpr (requires { typename T::CTTI_Tags; }) {
         // Checked internally, T has to be a complete type             
         return typename T::CTTI_Tags {};
      }
      else return NoTypes {};
   };
}

namespace Langulus
{
   /// Get the reflected tags, void if none                                   
   template<class T>
   using TagsOf = decltype(CT::Inner::GetTags<Decvq<Deref<T>>>());
}
