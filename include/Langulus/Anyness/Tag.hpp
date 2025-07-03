#pragma once
#include "Many.hpp"
#include "../../../source/rtti/MetaTag.hpp"
#include <Langulus/Tag.hpp>


namespace Langulus::Anyness
{

   ///                                                                        
   /// A type-erased dynamic tag, that depends on Anyness::Many               
   ///                                                                        
   struct Tag : ::Langulus::Inner::Tag<Many> {
      using ::Langulus::Inner::Tag<Many>::Tag;
   };

   ///                                                                        
   /// A statically typed tag                                                 
   ///                                                                        
   template<CT::NotVoid T>
   struct TTag : ::Langulus::Inner::Tag<T> {
      using ::Langulus::Inner::Tag<T>::Tag;
   };

} // namespace Langulus::Anyness


/// Define a tag, both in a type-erased and templated form                    
#define LANGULUS_DEFINE_TAG(NAME)               \
   namespace Langulus::Tags {                   \
      struct NAME : Anyness::Tag {              \
         using CTTI_DefineTag = Yes<#NAME>;     \
         using Anyness::Tag::Tag;               \
      };                                        \
      template<CT::NotVoid T>                   \
      struct T##NAME : Anyness::TTag<T> {       \
         using CTTI_DefineTag = Yes<#NAME>;     \
         using Anyness::TTag<T>::TTag;          \
      };                                        \
   }

LANGULUS_DEFINE_TAG(Name);
LANGULUS_DEFINE_TAG(Count);
