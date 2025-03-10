#pragma once
#include "../Literal.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::DefineTag<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_DefineTrait = YesText<"TraitID">;` in T    
   template<class T>
   struct DefineTag {
      static constexpr Literal Name = "<not a tag>";
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

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

} // namespace Langulus::RTTI
