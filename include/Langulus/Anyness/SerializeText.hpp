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
#include "TMap.hpp"


namespace Langulus::CTTI
{
   /// A rule for serializing any deep container that contains multiple items.
   /// This includes Many, Map, Set, Neat etc...                              
   /// as well as their templated equivalents. It basically places scopes,    
   /// separators and state decorators, depending on the kind of container.   
   template<CT::Deep C>
   void SerializationRule<Anyness::Text, C>::Serialize(
      C const& self, Anyness::Text& out, Context* context
   ) requires CT::ContainsMany<C> {
      static_assert(CT::NotHandle<C>);
      S::BeginScope(self, out, context);

      size_t counter = 0;
      self.Apply([&](auto const& item) {
         if constexpr (CT::Supported<decltype(item)>) {
            if (counter)
               S::Separate(self, out, context);

            Langulus::Serialize(item, out, context);
            ++counter;
         }
      });

      S::EndScope(self, out, context);

      if constexpr (requires { self.IsPast(); }) {
         if (self.IsPast())
            out += Serial::Past;
         else if (self.IsFuture())
            out += Serial::Future;
      }
   }

   /// A rule for serializing any deep container that contains single item.   
   /// This includes Any, Handle, Own, Ref, Pair and their templated          
   /// equivalents. Notice that Pair technically contains one item, but with  
   /// two dimensions.                                                        
   template<CT::Deep C>
   void SerializationRule<Anyness::Text, C>::Serialize(
      C const& self, Anyness::Text& out, Context* context
   ) requires CT::ContainsOne<C> {
      // Iterate all dimensions                                         
      bool first = true;
      C::Dimensions::ForEach([&]<uint ID> {
         if (first) first = false;
         else out += ": ";                // Dimension separator        

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Serialize a type-erased container                        
            const auto T = self.template GetType<ID>();
            const auto text_meta = MetaDataOf<Anyness::Text>();
            const auto serializer = T.GetMorphism(text_meta).serialize;
            try {
               LglsAssert(serializer, "Missing serializer",
                  " from ", T.GetName(), " to ", text_meta.GetName());
               serializer(DecvqAllCast(self.template GetRaw<ID>()), &out, context);
            }
            catch (...) {
               // Catch everything so that we can close any scopes      
               // Text serialization has non-fatal failures             
               S::Error(T, ID, out, context);
            }

            if (T.Is(MetaDataOf<Anyness::Any>())) {
               // Anyness::Any is the only container that matches       
               // the requirements: CT::Deep<T> and CT::ContainsOne<T>  
               // and having past/future state. Note: TAny is reflected 
               // as Any and is binary compatible as well.              
               auto const& item = self.template Get<Anyness::Any, ID>();
               if (item.IsPast())
                  out += Serial::Past;
               else if (item.IsFuture())
                  out += Serial::Future;
            }
         }
         else {
            //                                                          
            // Serialize a statically-typed container                   
            using T = Decay<TypeOf<C, ID>>;
            auto const& item = self.template Get<T, ID>();
            try {
               Langulus::Serialize(item, out, context);
            }
            catch (...) {
               // Catch everything so that we can close any scopes      
               // Text serialization has non-fatal failures             
               S::Error(MetaDataOf<T>(), ID, out, context);
            }

            if constexpr (CT::Deep<T> and CT::ContainsOne<T> and requires { item.IsPast(); }) {
               static_assert(CT::NotHandle<T>);
               if (item.IsPast())
                  out += Serial::Past;
               else if (item.IsFuture())
                  out += Serial::Future;
            }
         }
      });
   }

   /// Rule for serializing Code to Text. Wraps it in {} symbols.             
   inline void SerializationRule<Anyness::Text, Anyness::Code>::Serialize(
      const Anyness::Code& item, Anyness::Text& out, Context*
   ) {
      out += Serial::OpenCode;
      out += item;
      out += Serial::CloseCode;
   }
   
   /// Rule for serializing Text to Text. Wraps it in "".                     
   inline void SerializationRule<Anyness::Text, Anyness::Text>::Serialize(
      const Anyness::Text& item, Anyness::Text& out, Context*
   ) {
      out += Serial::OpenString;
      out += item;
      out += Serial::CloseString;
   }
   
   /// Rule for serializing characters to Text. Wraps them in ''.             
   template<CT::Character C>
   void SerializationRule<Anyness::Text, C>::Serialize(
      C const& item, Anyness::Text& out, Context*
   ) {
      out += Serial::OpenCharacter;
      out += item;
      out += Serial::CloseCharacter;
   }

   /// Rule for serializing Bytes to Text. Prepends 0x.                       
   inline void SerializationRule<Anyness::Text, Anyness::Bytes>::Serialize(
      const Anyness::Bytes& item, Anyness::Text& out, Context*
   ) {
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
   constexpr auto Converter<Anyness::Any, Anyness::Text>::Convert(
      Anyness::Any const& from
   ) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
   
   /// Convert TAny -> Text                                                   
   template<class T>
   constexpr auto Converter<Anyness::TAny<T>, Anyness::Text>::Convert(
      Anyness::TAny<T> const& from
   ) -> Anyness::Text {
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
   constexpr auto Converter<Anyness::Many, Anyness::Text>::Convert(
      Anyness::Many const& from
   ) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
   
   /// Convert TMany -> Text                                                  
   template<class T>
   constexpr auto Converter<Anyness::TMany<T>, Anyness::Text>::Convert(
      Anyness::TMany<T> const& from
   ) -> Anyness::Text {
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
   template<Anyness::StateValue SORT>
   constexpr auto Converter<Anyness::Inner::Set<SORT>, Anyness::Text>::Convert(
      Anyness::Inner::Set<SORT> const& from
   ) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
   
   /// Convert TSet -> Text                                                   
   template<CT::NotVoid T, Anyness::StateValue SORT>
   constexpr auto Converter<Anyness::TSet<T, SORT>, Anyness::Text>::Convert(
      Anyness::TSet<T, SORT> const& from
   ) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }


   
   ///                                                                        
   /// Map/TMap                                                               
   ///                                                                        

   /// Convert Map -> Text                                                    
   template<Anyness::StateValue SORT>
   constexpr auto Converter<Anyness::Inner::Map<SORT>, Anyness::Text>::Convert(
      Anyness::Inner::Map<SORT> const& from
   ) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
   
   /// Convert TMap -> Text                                                   
   template<CT::NotVoid K, CT::NotVoid V, Anyness::StateValue SORT>
   constexpr auto Converter<Anyness::TMap<K, V, SORT>, Anyness::Text>::Convert(
      Anyness::TMap<K, V, SORT> const& from
   ) -> Anyness::Text {
      if (from.IsEmpty())
         return {};

      Anyness::Text result;
      Serialize(from, result);
      return result;
   }
}
