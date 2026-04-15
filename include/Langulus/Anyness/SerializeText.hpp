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
#include "TMany.hpp"
#include "TSet.hpp"


namespace Langulus::CTTI
{
   /// A rule for serializing any deep container that contains multiple items.
   /// This includes Text, Bytes, Many, Map, Set, Pair, Neat, Tag, etc...     
   /// as well as their templated equivalents. It basically places scopes,    
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
               decltype(auto) element = self.GetAt(i);
               if constexpr (CT::Typed<C>)
                  serializer(&element, &out, context);
               else
                  serializer(const_cast<void*>(element), &out, context);

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

   /// A rule for serializing any deep container that contains single item.   
   /// This includes Any, Handle, Own, Ref, Pair and their templated          
   /// equivalents. Notice that Pair technically contains one item, but with  
   /// two dimensions.                                                        
   template<CT::Deep C>
   void SerializationRule<Anyness::Text, C>::Serialize(C const& self, Anyness::Text& out, Context* context)
   requires CT::ContainsOne<C> {
      C::Dimensions::ForEach([&]<uint ID> {
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Serialize a type-erased container                        
            const auto T = self.template GetType<ID>();
            if (T.IsDeep()) {
               auto item = self.template As<typename C::DeepType, ID>();
               try { Langulus::Serialize(item, out, context); }
               catch (...) {
                  
               }
            }
            else {
               const auto text_meta = MetaDataOf<Anyness::Text>();
               const auto serializer = T.GetMorphism(text_meta).serialize;
               LglsAssert(serializer, "Missing serializer",
                  " from ", T.GetName(), " to ", text_meta.GetName());
               serializer(DecvqAllCast(self.template Get<void, ID>()), &out, context);
            }
         }
         else {
            //                                                          
            // Serialize a statically-typed container                   
            using T = TypeOf<C, ID>;
            Decay<T> const& item = DenseCast(self.template Get<void, ID>());
            Langulus::Serialize(item, out, context);
         }
      });
   }

   /// Rule for serializing Code to Text. Wraps it in {} symbols.             
   inline void SerializationRule<Anyness::Text, Anyness::Code>::Serialize(const Anyness::Code& item, Anyness::Text& out, Context*) {
      out += Serial::OpenCode;
      out += item;
      out += Serial::CloseCode;
   }
   
   /// Rule for serializing Text to Text. Wraps it in "".                     
   inline void SerializationRule<Anyness::Text, Anyness::Text>::Serialize(const Anyness::Text& item, Anyness::Text& out, Context*) {
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
   inline void SerializationRule<Anyness::Text, Anyness::Bytes>::Serialize(const Anyness::Bytes& item, Anyness::Text& out, Context*) {
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
   


   ///                                                                        
   /// Any/TAny                                                               
   ///                                                                        
   
   /// Convert Any -> Text                                                    
   constexpr auto Converter<Anyness::Any, Anyness::Text>::Convert(Anyness::Any const& from) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
   
   /// Convert TAny -> Text                                                   
   template<class T>
   constexpr auto Converter<Anyness::TAny<T>, Anyness::Text>::Convert(Anyness::TAny<T> const& from) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }



   ///                                                                        
   /// Many/TMany                                                             
   ///                                                                        

   /// Convert Many -> Text                                                   
   constexpr auto Converter<Anyness::Many, Anyness::Text>::Convert(Anyness::Many const& from) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
   
   /// Convert TMany -> Text                                                  
   template<class T>
   constexpr auto Converter<Anyness::TMany<T>, Anyness::Text>::Convert(Anyness::TMany<T> const& from) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }



   ///                                                                        
   /// Set/TSet                                                               
   ///                                                                        

   /// Convert Set -> Text                                                    
   template<Anyness::State::StateValue SORT>
   constexpr auto Converter<Anyness::Inner::Set<SORT>, Anyness::Text>::Convert(Anyness::Inner::Set<SORT> const& from) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
   
   /// Convert TSet -> Text                                                   
   template<CT::NotVoid T, Anyness::State::StateValue SORT>
   constexpr auto Converter<Anyness::TSet<T, SORT>, Anyness::Text>::Convert(Anyness::TSet<T, SORT> const& from) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
}
