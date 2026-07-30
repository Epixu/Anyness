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
   /*template<CT::Deep C>
   void SerializationRule<Anyness::Bytes, C>::Serialize(
      ConstAll<C&> may_be_sparse, Anyness::Bytes& out, Context* context
   ) requires CT::ContainsMany<Decay<C>> {
      using DC = Decay<C>;
      static_assert(CT::NotHandle<DC>);
      DC const& self = DenseCast(may_be_sparse);
      S::BeginScope(self, out, context);

      self.Apply([&](auto const& item) {
         Langulus::Serialize(item, out, context);
      });

      S::EndScope(self, out, context);
   }*/

   /// A rule for serializing any deep container that contains single item.   
   /// This includes Any, Handle, Own, Ref and their templated equivalents.   
   /*template<CT::Deep C>
   void SerializationRule<Anyness::Bytes, C>::Serialize(
      ConstAll<C&> may_be_sparse, Anyness::Bytes& out, Context* context
   ) requires CT::ContainsOne<Decay<C>> {
      using DC = Decay<C>;
      DC const& self = DenseCast(may_be_sparse);
      S::BeginScope(self, out, context);

      // Iterate all dimensions                                         
      DC::Dimensions::ForEach([&]<uint ID> {
         if constexpr (CT::TypeErased<DC>) {
            //                                                          
            // Serialize a type-erased container                        
            const auto T = self.template GetType<ID>();
            const auto bytes_meta = MetaDataOf<Anyness::Bytes>();
            const auto serializer = T.GetMorphism(bytes_meta).serialize;
            LglsAssert(serializer, "Missing serializer",
               " from ", T.GetName(), " to ", bytes_meta.GetName());
            serializer(DecvqAllCast(self.template GetRaw<ID>()), &out, context);
         }
         else {
            //                                                          
            // Serialize a statically-typed container                   
            using T = Decay<TypeOf<DC, ID>>;
            auto* item = self.template Get<T, ID>();
            Langulus::Serialize(*item, out, context);
         }

         S::EndScope(self, out, context);
      });
   }*/

   /// Rule for serializing any container to bytes, deep or sparse            
   template<class C> requires CT::Container<Decay<C>>
   void SerializationRule<Anyness::Bytes, C>::Serialize(
      ConstAll<C&> may_be_sparse, Anyness::Bytes& out, Context* context
   ) {
      using DC = Decay<C>;
      DC const& self = DenseCast(may_be_sparse);
      S::BeginScope(self, out, context);

      if constexpr (CT::TypeErased<DC>) {
         //                                                             
         // Serialize a type-erased container                           
         const auto T = self.GetType();
         const auto bytes_meta = MetaDataOf<Anyness::Bytes>();
         auto serializer = T.GetMorphism(bytes_meta).serialize;
         LglsAssert(serializer, "Missing serializer",
            " from ", T.GetName(), " to ", bytes_meta.GetName());

         self.Apply([&](auto const& item) {
            serializer(item.GetRaw(), &out, context);
         });
      }
      else {
         //                                                             
         // Serialize a statically-typed container                      
         self.Apply([&](auto const& item) {
            Langulus::Serialize(*item, out, context);
         });
      }

      S::EndScope(self, out, context);
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
