///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Bytes.hpp"
#include "TAny.hpp"
#include "TMany.hpp"
#include <Langulus/HashOf.hpp>


namespace Langulus::CTTI
{
   /// A rule for serializing any deep container that contains multiple items.
   /// This includes Text, Bytes, Many, Map, Set, Pair, Neat, Tag, etc...     
   /// as well as their templated equivalents.                                
   template<CT::Deep C>
   void SerializationRule<Anyness::Bytes, C>::Serialize(
      C const& self, Anyness::Bytes& out, Context* context
   ) requires CT::ContainsMany<C> {
      if constexpr (CT::TypeErased<C>) {
         //                                                             
         // Serialize a type-erased container                           
         const auto T = self.GetType();
         if (T.IsDeep()) {
            for (Count i = 0; i < self.GetCount(); ++i) {
               auto item = self.template AsAt<typename C::DeepType>(i);
               S::BeginScope(item, out, context);
               Langulus::Serialize(item, out, context);
               S::EndScope(item, out, context);
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
            }
         }
         else {
            for (Count i = 0; i < self.GetCount(); ++i) {
               Decay<T> const& item = DenseCast(self[i]);
               Langulus::Serialize(item, out, context);
            }
         }
      }
   }

   /// A rule for serializing any deep container that contains single item.   
   /// This includes Any, Handle, Own, Ref and their templated equivalents.   
   template<CT::Deep C>
   void SerializationRule<Anyness::Bytes, C>::Serialize(
      C const& self, Anyness::Bytes& out, Context* context
   ) requires CT::ContainsOne<C> {
      if constexpr (CT::TypeErased<C>) {
         //                                                             
         // Serialize a type-erased container                           
         const auto T = self.GetType();
         if (T.IsDeep()) {
            auto item = self.template As<typename C::DeepType>();
            Langulus::Serialize(item, out, context);;
         }
         else {
            const auto text_meta = MetaDataOf<Anyness::Text>();
            const auto serializer = T.GetMorphism(text_meta).serialize;
            LglsAssert(serializer, "Missing serializer",
               " from ", T.GetName(), " to ", text_meta.GetName());
            serializer(DecvqAllCast(self.Get()), &out, context);
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

   /// Rule for serializing DMeta to Bytes                                    
   inline void SerializationRule<Anyness::Bytes, RTTI::DMeta>::Serialize(
      RTTI::DMeta const& item, Anyness::Bytes& out, Context* context
   ) {
      if (not item) {
         out += uint32_t {0};
         return;
      }

      auto registered = context->mDMetaBank.mDefinitions.find(item);
      if (registered == context->mDMetaBank.mDefinitions.end()) {
         context->mDMetaBank.mDefinitions[item] = context->mDMetaBank.mNextId;
         out += context->mDMetaBank.mNextId;
         ++context->mDMetaBank.mNextId;
      }
      else out += registered->second;
   }

   /// Rule for serializing TMeta to Bytes                                    
   inline void SerializationRule<Anyness::Bytes, RTTI::TMeta>::Serialize(
      RTTI::TMeta const& item, Anyness::Bytes& out, Context* context
   ) {
      if (not item) {
         out += uint32_t {0};
         return;
      }

      auto registered = context->mTMetaBank.mDefinitions.find(item);
      if (registered == context->mTMetaBank.mDefinitions.end()) {
         context->mTMetaBank.mDefinitions[item] = context->mTMetaBank.mNextId;
         out += context->mTMetaBank.mNextId;
         ++context->mTMetaBank.mNextId;
      }
      else out += registered->second;
   }

   /// Rule for serializing CMeta to Bytes                                    
   inline void SerializationRule<Anyness::Bytes, RTTI::CMeta>::Serialize(
      RTTI::CMeta const& item, Anyness::Bytes& out, Context* context
   ) {
      if (not item) {
         out += uint32_t {0};
         return;
      }

      auto registered = context->mCMetaBank.mDefinitions.find(item);
      if (registered == context->mCMetaBank.mDefinitions.end()) {
         context->mCMetaBank.mDefinitions[item] = context->mCMetaBank.mNextId;
         out += context->mCMetaBank.mNextId;
         ++context->mCMetaBank.mNextId;
      }
      else out += registered->second;
   }

   /// Rule for serializing VMeta to Bytes                                    
   inline void SerializationRule<Anyness::Bytes, RTTI::VMeta>::Serialize(
      RTTI::VMeta const& item, Anyness::Bytes& out, Context* context
   ) {
      if (not item) {
         out += uint32_t {0};
         return;
      }

      auto registered = context->mVMetaBank.mDefinitions.find(item);
      if (registered == context->mVMetaBank.mDefinitions.end()) {
         context->mVMetaBank.mDefinitions[item] = context->mVMetaBank.mNextId;
         out += context->mVMetaBank.mNextId;
         ++context->mVMetaBank.mNextId;
      }
      else out += registered->second;
   }
}
