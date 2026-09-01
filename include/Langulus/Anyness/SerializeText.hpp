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
#include "Any.hpp"
#include "TAny.hpp"
#include "Many.hpp"
#include "TMany.hpp"
#include "Set.hpp"
#include "TSet.hpp"
#include "Pair.hpp"
#include "TPair.hpp"
#include "Map.hpp"
#include "TMap.hpp"


namespace Langulus::CTTI
{
   /// MARK: Serialize many                                                   
   /// A rule for serializing any deep container that contains multiple items.
   /// This includes Many, Map, Set, Neat etc...                              
   /// as well as their templated equivalents. It basically places scopes,    
   /// separators and state decorators, depending on the kind of container.   
   template<CT::Deep C>
   void SerializationRule<Anyness::Text, C>::Serialize(
      ConstAll<C&> may_be_sparse, Anyness::Text& out, Context* context
   ) requires CT::ContainsMany<Decay<C>> {
      using DC = Decay<C>;
      static_assert(CT::NotHandle<DC>);
      DC const& self = DenseCast(may_be_sparse);
      S::BeginScope(self, out, context);

      size_t counter = 0;
      self.Apply([&](auto const& item) {
         if (counter)
            S::Separate(self, out, context);

         Langulus::Serialize(item, out, context);
         ++counter;
      });

      S::EndScope(self, out, context);

      if constexpr (requires { self.IsPast(); }) {
         if (self.IsPast())
            out += Serial::Past;
         else if (self.IsFuture())
            out += Serial::Future;
      }
   }

   /// MARK: Serialize one                                                    
   /// A rule for serializing any deep container that contains single item.   
   /// This includes Any, Handle, Own, Ref, Pair and their templated          
   /// equivalents. Notice that Pair technically contains one item, but with  
   /// two dimensions.                                                        
   template<CT::Deep C>
   void SerializationRule<Anyness::Text, C>::Serialize(
      ConstAll<C&> may_be_sparse, Anyness::Text& out, Context* context
   ) requires CT::ContainsOne<Decay<C>> {
      using DC = Decay<C>;
      DC const& self = DenseCast(may_be_sparse);

      // Iterate all dimensions                                         
      bool first = true;
      DC::Dimensions::ForEach([&]<uint ID> {
         if (first) first = false;
         else out += ": ";                // Dimension separator        

         if constexpr (CT::TypeErased<DC>) {
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
               auto* item = self.template Get<Anyness::Any, ID>();
               if (item->IsPast())
                  out += Serial::Past;
               else if (item->IsFuture())
                  out += Serial::Future;
            }
         }
         else {
            //                                                          
            // Serialize a statically-typed container                   
            using T = Decay<TypeOf<DC, ID>>;
            auto* item = self.template Get<T, ID>();
            try {
               Langulus::Serialize(*item, out, context);
            }
            catch (...) {
               // Catch everything so that we can close any scopes      
               // Text serialization has non-fatal failures             
               S::Error(MetaDataOf<T>(), ID, out, context);
            }

            if constexpr (CT::Deep<T> and CT::ContainsOne<T> and requires { item->IsPast(); }) {
               static_assert(CT::NotHandle<T>);
               if (item->IsPast())
                  out += Serial::Past;
               else if (item->IsFuture())
                  out += Serial::Future;
            }
         }
      });
   }

   /// MARK: Code                                                             
   /// Rule for serializing Code to Text. Wraps it in {} symbols.             
   template<CT::Container C> requires (not CT::Deep<C>)
   void SerializationRule<Anyness::Text, C>::Serialize(
      ConstAll<C&> item, Anyness::Text& out, [[maybe_unused]] Context* context
   ) {
      if constexpr (Same<C, Anyness::Code>) {
         out += Serial::OpenCode;
         out += item;
         out += Serial::CloseCode;
      }
      else if constexpr (Same<C, Anyness::Text>) {
         out += Serial::OpenString;
         out += item;
         out += Serial::CloseString;
      }
      else if constexpr (Same<C, Anyness::Bytes>) {
         out += Serial::OpenByte;
         out.Reserve(item.GetCount()*2);
         ::std::array<char, sizeof(Byte) * 2> temp;
         auto from = item.GetRaw();
         const auto fromEnd = item.GetRawEnd();
         while (from != fromEnd) {
            ::fmt::format_to_n(temp.data(), 2, "{:02X}", from->value);
            out += Anyness::Text(temp);
            ++from;
         }
         out += Serial::CloseByte;   
      }
      else {
         static_assert(false, "Unhandled non-deep container");
         /*if (item.IsEmpty()) // risk of infinite recursion
            return;

         Anyness::Text result;
         Serialize(item, result, context);
         out += result;*/
      }
   }
   
   /// MARK: Characters                                                       
   /// Rule for serializing characters to Text. Wraps them in ''.             
   template<CT::Character C>
   void SerializationRule<Anyness::Text, C>::Serialize(
      C const& item, Anyness::Text& out, Context*
   ) {
      out += Serial::OpenCharacter;
      out += item;
      out += Serial::CloseCharacter;
   }
}

namespace fmt
{
   /// MARK: {fmt}                                                            
   /// Extend FMT to be capable of logging any Anyness container that is      
   /// convertible to Anyness::Text.                                          
   template<::Langulus::CT::Container T> requires ::Langulus::CT::Convertible<T, ::Langulus::Anyness::Text>
   struct formatter<T> {
      template<class CONTEXT>
      constexpr auto parse(CONTEXT& ctx) {
         return ctx.begin();
      }

      template<class CONTEXT>
      auto format(T const& e, CONTEXT& ctx) const {
         try {
            const auto to_text = ::Langulus::Convert<::Langulus::Anyness::Text>(e);
            return format_to(ctx.out(), "{}", static_cast<::Langulus::Token>(to_text));
         }
         catch(...) {
            // Don't allow any exceptions to leak out of here           
            return format_to(ctx.out(), "<error while serializing to text>");
         }
      }
   };
}