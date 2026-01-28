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
   /// Can be used in two ways to satisfy CT::Serializer<T>:                  
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `struct CTTI_Serializer {...};` in T                   
   template<class T>
   struct Serializer;
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Helper function to extract reflected serialize                      
      template<class T>
      consteval auto GetSerializer() {
         static_assert(not ::std::is_reference_v<T>,
            "Strip references first");

         if constexpr (CT::Complete<CTTI::Serializer<T>>) {
            // Checked externally, T doesn't have to be complete        
            return CTTI::Serializer<T> {};
         }
         else if constexpr (requires { typename T::CTTI_Serializer; }) {
            // Checked internally, T has to be a complete type          
            return typename T::CTTI_Serializer {};
         }
         else return NoTypes {};
      };
   }

   /// Check if all T are serializers                                         
   template<class...T>
   concept Serializer = PartialValidate<T...>
       and ((not Void<decltype(Inner::GetSerializer<Shed<T>>())>) and ...);
}

namespace Langulus::CTTI
{
   /// This can be specialized for custom serialization rules. If a morphism  
   /// doesn't have custom rules, a static_cast<S>(T) is done, and then       
   /// the result is concatenated to the back of S.                           
   template<CT::Serializer S, class T>
   struct SerializationRule;
}

namespace Langulus
{
   /// Get the reflected serializer, CT::Void if none                         
   template<class T>
   using SerializerOf = decltype(CT::Inner::GetSerializer<Shed<T>>());

   /// Serialize                                                              
   template<class FROM, CT::Serializer TO>
   auto Serialize(FROM& from, TO& to, typename SerializerOf<TO>::Context* context = nullptr)
   -> typename TO::CountType {
      using DFROM = DecvqAll<FROM>;
      using DTO   = DecvqAll<TO>;
      const typename DTO::CountType initial = to.GetCount();
      
      if constexpr (CT::Complete<CTTI::SerializationRule<DTO, DFROM>>) {
         // Custom rule exists                                          
         CTTI::SerializationRule<DTO, DFROM>::Serialize(from, to, context);
      }
      else {
         // No rule exists, just cast and concatenate, if possible      
         (void) context;
         to += Convert<DTO>(from);
      }
      return to.GetCount() - initial;
   }
}

namespace Langulus::Serial
{
   /// Helps to define an operator                                            
   struct Operator {
      Token mToken;
      bool mCharge = false;
   };

   /// Built-in operator properties.                                          
   /// These are tuned for Langulus::Code specification, but you can          
   /// use your own in your custom CTTI_Serializer.                           
   constexpr Operator OpenScope      { "(" };
   constexpr Operator CloseScope     { ")" };
   constexpr Operator OpenScopeAlt   { "[" };
   constexpr Operator CloseScopeAlt  { "]" };
   constexpr Operator OpenCode       { "{" };
   constexpr Operator CloseCode      { "}" };
   constexpr Operator OpenComment    { "/*" };
   constexpr Operator CloseComment   { "*/" };
   constexpr Operator LineComment    { "//" };
   constexpr Operator OpenString     { "\"" };
   constexpr Operator CloseString    { "\"" };
   constexpr Operator OpenStringAlt  { "`" };
   constexpr Operator CloseStringAlt { "`" };
   constexpr Operator OpenCharacter  { "'" };
   constexpr Operator CloseCharacter { "'" };
   constexpr Operator OpenByte       { "0x" };
   constexpr Operator CloseByte      { "" };
   constexpr Operator SelectIdea     { "##" };
   constexpr Operator SelectThing    { "#" };
   constexpr Operator Future         { "??" };
   constexpr Operator Past           { "?" };
   constexpr Operator Null           { "null" };
   constexpr Operator Mass           { "*", true };
   constexpr Operator Rate           { "^", true };
   constexpr Operator Time           { "@", true };
   constexpr Operator Priority       { "!", true };
   constexpr Operator And            { ", " };
   constexpr Operator AndUnordered   { "; " };
   constexpr Operator Or             { " or " };
}
