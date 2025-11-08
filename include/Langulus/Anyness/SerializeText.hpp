///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Text.hpp"
#include "Bytes.hpp"
#include "TAny.hpp"


namespace Langulus::CTTI
{
   /// A rule for serializing any deep container.                             
   /// This includes Any, Many, Map, Set, Pair, Neat, Tag, etc...             
   /// as well as any templated equivalents. It basically places scopes,      
   /// separators and state decorators, depending on the kind of container.   
   template<CT::Deep C>
   void SerializationRule<Anyness::Text, C>::Serialize(C const& self, Anyness::Text& out, Context* context)
   requires CT::ContainsMany<C> {
      if constexpr (CT::TypeErased<C>) {
         //                                                             
         // Serialize a type-erased container                           
         const auto T = self.GetType();
         if (T.IsDeep()) {
            for (Count i = 0; i < self.GetCount(); ++i) {
               auto item = self.template AsAt<typename C::DeepType>(i);
               S::BeginScope(item, out, context);
               try { Langulus::Serialize(item, out, context); }
               catch (...) {
                     
               }
               S::EndScope(item, out, context);

               if (i < self.GetCount() - 1)
                  S::Separate(self, out, context);
            }
         }
         else {
            const auto text_meta = MetaDataOf<Anyness::Text>();
            auto serializer = T.GetMorphism(MetaDataOf<Anyness::Text>()).serialize;
            LglsAssert(serializer, "Missing serializer",
               " from ", T.GetName(), " to ", text_meta.GetName());

            for (Count i = 0; i < self.GetCount(); ++i) {
               serializer(self.GetAt(i), &out, context);

               if (i < self.GetCount() - 1)
                  S::Separate(self, out, context);
            }                  
         }
      }
      else {
         //                                                             
         // Serialize a statically-typed container                      
         using T = TypeOf<C>;
         if constexpr (CT::Deep<T>) {
            for (Count i = 0; i < self.GetCount(); ++i) {
               Decay<T> const& item = DenseCast(self[i]);
               S::BeginScope(item, out, context);
               Langulus::Serialize(item, out, context);
               S::EndScope(item, out, context);

               if (i < self.GetCount() - 1)
                  S::Separate(self, out, context);
            }
         }
         else {
            for (Count i = 0; i < self.GetCount(); ++i) {
               Decay<T> const& item = DenseCast(self[i]);
               Langulus::Serialize(item, out, context);

               if (i < self.GetCount() - 1)
                  S::Separate(self, out, context);
            }
         }
      }
   }
      
   template<CT::Deep C>
   void SerializationRule<Anyness::Text, C>::Serialize(C const& self, Anyness::Text& out, Context* context)
   requires CT::ContainsOne<C> {
      if constexpr (CT::TypeErased<C>) {
         //                                                             
         // Serialize a type-erased container                           
         const auto T = self.GetType();
         if (T.IsDeep()) {
            auto item = self.template As<typename C::DeepType>();
            try { Langulus::Serialize(item, out, context); }
            catch (...) {
                  
            }
         }
         else {
            const auto text_meta = MetaDataOf<Anyness::Text>();
            const auto serializer = T.GetMorphism(text_meta).serialize;
            LglsAssert(serializer, "Missing serializer",
               " from ", T.GetName(), " to ", text_meta.GetName());
            serializer(self.Get(), &out, context);
         }
      }
      else {
         //                                                             
         // Serialize a statically-typed container                      
         using T = TypeOf<C>;
         if constexpr (CT::Deep<T>) {
            Decay<T> const& item = DenseCast(*self);
            Langulus::Serialize(item, out, context);
         }
         else {
            Decay<T> const& item = DenseCast(*self);
            Langulus::Serialize(item, out, context);
         }
      }
   }

   /// Rule for serializing Code to Text. Wraps it in {} symbols.             
   void SerializationRule<Anyness::Text, Anyness::Code>::Serialize(const Anyness::Code& item, Anyness::Text& out, Context*) {
      out += Serial::OpenCode;
      out += item;
      out += Serial::CloseCode;
   }
   
   /// Rule for serializing Text to Text. Wraps it in "".                     
   void SerializationRule<Anyness::Text, Anyness::Text>::Serialize(const Anyness::Text& item, Anyness::Text& out, Context*) {
      out += Serial::OpenString;
      out += item;
      out += Serial::CloseString;
   }
   
   /// Rule for serializing characters to Text. Wraps them in ''.             
   template<CT::Character C>
   void SerializationRule<Anyness::Text, C>::Serialize(C const& item, Anyness::Text& out, Context*) {
      out += Serial::OpenCharacter;
      out += item;
      out += Serial::CloseCharacter;
   }

   /// Rule for serializing Bytes to Text. Prepends 0x.                       
   void SerializationRule<Anyness::Text, Anyness::Bytes>::Serialize(const Anyness::Bytes& item, Anyness::Text& out, Context*) {
      out += Serial::OpenByte;
      out.Reserve(item.GetCount()*2);
      ::std::array<char, sizeof(Byte) * 2> temp;
      auto from = item.GetRaw();
      const auto fromEnd = item.GetRawEnd();
      while (from != fromEnd) {
         ::fmt::format_to_n(temp.data(), 2, "{:02X}", from->value);
         out += temp;
         ++from;
      }
      out += Serial::CloseByte;
   }
   
   /// Convert Any -> Text                                                    
   constexpr void Converter<Anyness::Any, Anyness::Text>::Convert(Anyness::Any const& from, Anyness::Text& to) {
      Serialize(from, to);
   }
   
   constexpr auto Converter<Anyness::Any, Anyness::Text>::Convert(Anyness::Any const& from) -> Anyness::Text {
      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
   
   /// Convert TAny -> Text                                                   
   template<class T>
   constexpr void Converter<Anyness::TAny<T>, Anyness::Text>::Convert(Anyness::TAny<T> const& from, Anyness::Text& to) {
      Serialize(from, to);
   }

   template<class T>
   constexpr auto Converter<Anyness::TAny<T>, Anyness::Text>::Convert(Anyness::TAny<T> const& from) -> Anyness::Text {
      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
}
