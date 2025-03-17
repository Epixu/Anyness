#pragma once
#include "Many.hpp"
#include "../../../source/rtti/MetaTag.hpp"
#include <Langulus/Tag.hpp>


namespace Langulus::Anyness
{

   ///                                                                        
   /// A type-erased dynamic tag, that depends on Anyness::Many               
   ///                                                                        
   struct Tag : Inner::Tag<Many> {
      using Inner::Tag<Many>::Tag;
   };

   ///                                                                        
   /// A statically typed tag                                                 
   ///                                                                        
   template<CT::NotVoid T>
   struct TTag : Inner::Tag<T> {
      using Inner::Tag<T>::Tag;
   };

} // namespace Langulus::Anyness


/// Define a tag, both in a type-erased and templated form                    
#define LANGULUS_DEFINE_TAG(NAME)               \
   namespace Langulus::Tags {                   \
      struct NAME : Inner::Tag<Anyness::Many> { \
         using CTTI_DefineTag = YesText<#NAME>; \
         using Inner::Tag<Anyness::Many>::Tag;  \
      };                                        \
      template<CT::NotVoid T>                   \
      struct T##NAME : Inner::Tag<T> {          \
         using CTTI_DefineTag = YesText<#NAME>; \
         using Inner::Tag<T>::Tag;              \
      };                                        \
   }

LANGULUS_DEFINE_TAG(Name);
LANGULUS_DEFINE_TAG(Count);