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
   auto Serialize(FROM const& from, TO& to, typename TO::Context* context = nullptr)
   -> typename TO::CountType {
      const typename TO::CountType initial = to.GetCount();
      if constexpr (CT::Complete<CTTI::SerializationRule<TO, FROM>>) {
         // Custom rule exists                                          
         CTTI::SerializationRule<TO, FROM>::Serialize(from, to, context);
      }
      else {
         // No rule exists, just cast and concatenate                   
         (void) context;
         to += static_cast<TO>(from);
      }
      return to.GetCount() - initial;
   }
}
