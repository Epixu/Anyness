///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "../Main.hpp"
#include <Langulus/CT/Character.hpp>

using namespace Langulus;

namespace
{
   template<class T>
   struct SheddableType { using CTTI_Sheddable = T; };
   struct IncompleteType;

   struct CharacterExternally {};
   struct NotCharacterExternally {};
   struct CharacterInternally { using CTTI_Character = Yes<>; };
   struct InheritedCharacter : CharacterInternally {};
   struct InheritedCharacterDisabled : CharacterInternally { using CTTI_Character = No; };
   struct InheritedCharacterButPrivate : private CharacterInternally {};
   struct InheritedCharacterExternally : CharacterExternally {};
}

namespace Langulus::CTTI
{
   template<>
   struct Character<CharacterExternally> {};
   template<>
   struct Character<NotCharacterExternally> {
      static constexpr bool Enabled = false;
   };
}


///                                                                           
/// CT::Character                                                             
///                                                                           
TEMPLATE_TEST_CASE("Testing CT::Character types", "[ct]",
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //IncompleteType const&,         // shouldn't compile
   //SheddableType<IncompleteType>, // shouldn't compile
   //SheddableType<IncompleteType>&,// shouldn't compile
   CharacterExternally,
   CharacterExternally const,
   CharacterExternally&,
   CharacterInternally,
   CharacterInternally const,
   CharacterInternally&,
   InheritedCharacter,
   InheritedCharacter const,
   InheritedCharacter&,
   char, wchar_t, char8_t, char16_t, char32_t
) {
   static_assert(    CT::Character<TestType>);
   static_assert(not CT::NotCharacter<TestType>);
}

TEMPLATE_TEST_CASE("Testing CT::NotCharacter types", "[ct]",
   //IncompleteType,                // shouldn't compile
   //IncompleteType const,          // shouldn't compile
   //IncompleteType const&,         // shouldn't compile
   //SheddableType<IncompleteType>, // shouldn't compile
   //SheddableType<IncompleteType>&,// shouldn't compile
   IncompleteType*,
   bool,
   void, void*,
   int, int const, int const&, int&,
   char*, wchar_t*, char8_t*, char16_t*, char32_t*,
   Types<void*>,
   SheddableType<CharacterInternally*>,
   SheddableType<CharacterInternally* const>,
   SheddableType<CharacterInternally* const&>,
   SheddableType<InheritedCharacterDisabled>,
   SheddableType<InheritedCharacterDisabled const>,
   SheddableType<InheritedCharacterDisabled const&>,
   InheritedCharacterDisabled,
   InheritedCharacterExternally,
   InheritedCharacterButPrivate,
   NotCharacterExternally
) {
   static_assert(not CT::Character<TestType>);
   static_assert(    CT::NotCharacter<TestType>);
}

//static_assert(    CT::Character<>); // shouldn't compile at all
static_assert(    CT::Character<CharacterExternally, CharacterInternally, char>);
static_assert(not CT::Character<CharacterExternally, CharacterInternally, int>);

//static_assert(    CT::NotCharacter<>); // shouldn't compile at all
static_assert(    CT::NotCharacter<InheritedCharacterDisabled, InheritedCharacterExternally, int>);
static_assert(not CT::NotCharacter<InheritedCharacterDisabled, InheritedCharacterExternally, char>);
