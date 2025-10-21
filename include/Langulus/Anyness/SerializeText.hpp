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


namespace Langulus::CTTI
{   
   /// The presence of this structure makes Text a CT::Serializer             
   template<>
   struct Serializer<Anyness::Text> {
      using Text = Anyness::Text;
      using Count = Text::CountType;
      
      // Text serializer can be lossy to omit unnecessary details,      
      // and you can configure how many elements to show                
      #ifdef LANGULUS_MAX_DEBUGGABLE_ELEMENTS
         static constexpr Count MaxIterations = LANGULUS_MAX_DEBUGGABLE_ELEMENTS;
      #elif LANGULUS(DEBUG) or LANGULUS(SAFE)
         static constexpr Count MaxIterations = 32;
      #else
         static constexpr Count MaxIterations = 8;
      #endif

      struct Context {};
      
      static constexpr bool CriticalFailure = false;
      static constexpr bool SkipElements = true;

      static void BeginScope(const CT::Container auto& from, Text& to, Context*) {
         const bool scoped = from.GetCount() > 1 or from.IsInvalid() or from.IsExecutable(); //TODO could carry in context and check verb precedence to avoid scoping in some cases
         if (scoped) {
            if (from.IsPast())
               to += Serial::Past;
            else if (from.IsFuture())
               to += Serial::Future;

            to += Serial::OpenScope;
         }
      }
      
      static void EndScope(const CT::Container auto& from, Text& to, Context*) {
         const bool scoped = from.GetCount() > 1 or from.IsInvalid() or from.IsExecutable(); //TODO could carry in context and check verb precedence to avoid scoping in some cases
         if (scoped)
            to += Serial::CloseScope;
      }
      
      static void Separate(const CT::Container auto& from, Text& to, Context*) {
         to += (from.IsOr() ? " or " : ", ");
      }
      
      static void Empty(RTTI::DMeta type, Count i, Text& to, Context*) {
         if constexpr (CriticalFailure) {
            LglsError("Item #", i, " of type `", type.GetName(),
               "` was serialized to an empty `Text`");
         }
         else {
            to += "/*";
            to += type.GetName();
            to += " -> empty Text*/";            
         }
      }
      
      static void Error(RTTI::DMeta type, Count i, Text& to, Context*) {
         if constexpr (CriticalFailure) {
            LglsError("Item #", i, " of type `", type.GetName(),
               "` failed to convert to `Text`");
         }
         else {
            to += "/*";
            to += type.GetName();
            to += " -> Text failed*/";            
         }
      }
   };

   /// A rule for serializing any deep container.                             
   /// This includes Any, Many, Map, Set, Pair, Neat, Tag, etc...             
   /// as well as any templated equivalents. It basically places scopes,      
   /// separators and state decorators, depending on the kind of container.   
   template<CT::Deep C>
   struct SerializationRule<Anyness::Text, C> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;
      using Count = Anyness::Text::CountType;
      
      static void Serialize(C const& self, Anyness::Text& out, Context* context)
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
            if constexpr (CT::Deep<Decay<T>>) {
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
      
      static void Serialize(C const& self, Anyness::Text& out, Context* context)
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
            if constexpr (CT::Deep<Decay<T>>) {
               Decay<T> const& item = DenseCast(*self);
               Langulus::Serialize(item, out, context);
            }
            else {
               Decay<T> const& item = DenseCast(*self);
               Langulus::Serialize(item, out, context);
            }
         }
      }
   };

   /// Rule for serializing Code to Text. Wraps it in {} symbols.             
   template<>
   struct SerializationRule<Anyness::Text, Anyness::Code> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static void Serialize(const Anyness::Code& item, Anyness::Text& out, Context*) {
         out += Serial::OpenCode;
         out += item;
         out += Serial::CloseCode;
      }
   };
   
   /// Rule for serializing Text to Text. Wraps it in "".                     
   template<>
   struct SerializationRule<Anyness::Text, Anyness::Text> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static auto Serialize(const Anyness::Text& item, Anyness::Text& out, Context*) {
         out += Serial::OpenString;
         out += item;
         out += Serial::CloseString;
      }
   };
   
   /// Rule for serializing characters to Text. Wraps them in ''.             
   template<CT::Character C>
   struct SerializationRule<Anyness::Text, C> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static auto Serialize(C const& item, Anyness::Text& out, Context*) {
         out += Serial::OpenCharacter;
         out += item;
         out += Serial::CloseCharacter;
      }
   };

   /// Rule for serializing Bytes to Text. Prepends 0x.                       
   template<>
   struct SerializationRule<Anyness::Text, Anyness::Bytes> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static auto Serialize(const Anyness::Bytes& item, Anyness::Text& out, Context*) {
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
   };

   /// Convert Number -> Text                                                 
   template<CT::Number T>
   struct Converter<T, Anyness::Text> {
      static constexpr void Convert(T const& from, Anyness::Text& to) {
         to += Anyness::Text::FromNumber(from);
      }
      
      static constexpr Anyness::Text Convert(T const& from) {
         return Anyness::Text::FromNumber(from);
      }
   };
   
   /// Convert Any -> Text                                                    
   template<>
   struct Converter<Anyness::Any, Anyness::Text> {
      static constexpr void Convert(Anyness::Any const& from, Anyness::Text& to) {
         Serialize(from, to);
      }
      
      static constexpr Anyness::Text Convert(Anyness::Any const& from) {
         Anyness::Text result;
         Serialize(from, result);
         return result;
      }
   };
   
   /// Convert TAny -> Text                                                   
   template<class T>
   struct Converter<Anyness::TAny<T>, Anyness::Text> {
      static constexpr void Convert(Anyness::TAny<T> const& from, Anyness::Text& to) {
         Serialize(from, to);
      }
      
      static constexpr Anyness::Text Convert(Anyness::TAny<T> const& from) {
         Anyness::Text result;
         Serialize(from, result);
         return result;
      }
   };
}
