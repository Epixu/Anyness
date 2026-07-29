///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Handle.hpp"
#include "Langulus/IntentOf.hpp"
#include <source/components/Typed-Static.hpp>
#include <source/components/Heap-Movable.hpp>
#include <source/components/Count-Stack.hpp>
#include <source/components/Reserve-Emergent.hpp>
#include <source/components/Ownership-Stack.hpp>
#include <source/components/Hash-Stack.hpp>
#include <source/components/Insertion.hpp>
#include <source/components/InsertionOperators.hpp>
#include <source/components/InsertionOperatorsConcat.hpp>
#include <source/components/Removal.hpp>
#include <source/components/Assignment.hpp>
#include <source/components/Comparison.hpp>
#include <source/components/Conversion.hpp>
#include <source/components/IndexedLinear.hpp>
#include <source/components/Iteration-ForEach.hpp>
#include <source/components/Iteration-Range.hpp>
#include <source/states/Disowned.hpp>
#include <source/states/Compressed.hpp>
#include <source/states/Encrypted.hpp>
#include <Langulus/Utils/Byte.hpp>


namespace Langulus::Anyness
{
   struct Bytes;

   namespace Inner
   {
      using BytesBase = Com::Container<
         Com::State::Disowned<>,             // Allows disownment       
         Com::TypedStatic<DMeta, Byte>,      // Type-constrained        
         Com::HeapMovable<0, 0, HeapEntry<0, Byte*>>,
         Com::CountStack<>,                  // Variable count          
         Com::ReserveEmergent<>,             // Emergent reserve        
         Com::IndexedLinear<>,               // Indexed directly        
         Com::OwnershipStack<>,              // Allocation is referenced
         Com::HashStack<>,                   // Variable hash (cached)  
         Com::Insertion<true>,               // Serialize + insert      
         Com::InsertionOperators<>,          // << and >> insertion     
         Com::InsertionOperatorsConcat<>,    // + and += concat         
         Com::Removal<>,                     // Allows removal          
         Com::Assignment<true>,              // Allows assignment       
         Com::Comparison<true>,              // Allows for comparison   
         Com::Conversion<>,                  // Allows conversion       
         Com::IterationForEach<>,            // ForEach iteration       
         Com::IterationRange<>,              // Range iteration       😊
         Com::State::Compressed<>,           // Toggle compression      
         Com::State::Encrypted<>             // Toggle encryption       
      >;
   }
   

   ///                                                                        
   /// A continuous byte container of variable size                           
   ///                                                                        
   #pragma pack(push, 4)
   struct Bytes : Inner::BytesBase {
      using CTTI_ReflectAs = Bytes;
      using CTTI_MapsTo    = Text;
      using CTTI_MapsFrom  = Types<RTTI::DMeta, RTTI::TMeta, RTTI::CMeta, RTTI::VMeta>;

      using CountType = Base::CountType;

      constexpr Bytes() noexcept {
         this->ConstructDefault();
      }

      constexpr Bytes(nullptr_t) noexcept
         : Bytes {} {}

      constexpr Bytes(Bytes const& other)
         : Bytes {Refer {other}} {}

      constexpr Bytes(Bytes&& other) noexcept
         : Bytes {Move  {other}} {}

      constexpr ~Bytes() noexcept {
         this->Destroy();
      }
      
      /// Construction that absorbs the provided containers                   
      template<class A1, class...AN>
      constexpr Bytes(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Concat(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr Bytes(Inner::Piecewise, A1&& a1, AN&&...an) {
         this->ConstructDefault();
         this->Insert(LglsFwd(a1), LglsFwd(an)...);
      }

      /// Construction from any kind of other bytes with intent               
      template<template<class> class I> requires CT::Intent<I<Bytes>>
      constexpr Bytes(I<Bytes>&& bytes) {
         this->Absorb(LglsFwd(bytes));
      }

      /// Construction from any kind of POD value.                            
      /// Works for bounded arrays as well.                                   
      template<class T> requires CT::POD<DeextAll<Deint<T>>>
      explicit constexpr Bytes(T&& source) {
         this->ResetState();
         this->SetHeapInner(static_cast<const void*>(&DeintCast(source)));
         this->SetCountInner(sizeof(Deint<T>));
         this->ResetHash();

         // We may own this data                                        
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if constexpr (CT::Disowned<T> or CT::Copied<T> or CT::Cloned<T>)
               this->SetAllocationInner(nullptr);
            else
               this->FindAllocationInner();
         #else
            this->SetAllocationInner(nullptr);
         #endif

         // Take ownership if the intent requires it                    
         if constexpr (CT::Copied<T> or CT::Cloned<T>)
            this->TakeOwnership();
      }

      /// Construction from a byte                                            
      ///   @attention this is an owning constructor                          
      constexpr Bytes(Byte&& b) {
         this->ResetState();
         this->AllocateFresh(1);
         *this->GetRawAs<Byte>() = b;
         this->SetCountInner(1);
         this->ResetHash();
      }

      /// Assignment                                                          
      constexpr Bytes& operator = (Bytes const& other) {
         return this->AssignAbsorb(Refer {other});
      }
      constexpr Bytes& operator = (Bytes&& other) noexcept {
         return this->AssignAbsorb(Move {other});
      }
      
      /// Comparing against nullptr_t checks if text is empty                 
      /*constexpr bool operator == (nullptr_t) const noexcept {
         return this->IsEmpty();
      }*/

      /// Comparing against std containers with characters                    
      /*constexpr bool operator == (const CT::TextRange auto& rhs) const noexcept {
         return operator == (Text {Disown(rhs)});
      }

      /// Comparison                                                          
      constexpr auto operator <=> (CT::TextRange auto const& other) const noexcept -> ::std::partial_ordering {
         return this->Compare(other);
      }

      constexpr auto operator <=> (Bytes const& other) const noexcept -> ::std::partial_ordering {
         return this->Compare(other);
      }

      constexpr bool operator == (Bytes const& other) const noexcept {
         return this->CompareEqual(other);
      }*/

      /// Comparison                                                          
      /*constexpr auto operator <=> (Bytes const& other) const noexcept -> ::std::partial_ordering {
         return this->Compare(other);
      }
      constexpr bool operator == (Bytes const& other) const noexcept {
         return this->CompareEqual(other);
      }*/

      /// Conversion to standard string as a sequence of hex bytes            
      explicit operator ::std::string() const {
         if (this->IsEmpty())
            return {};

         ::std::string result;
         result.resize(this->GetCount() * 2);
         auto from_bytes = this->template GetRawAs<uint8_t>();
         auto to_bytes = result.data();
         for (size_t i = 0; i < result.size(); ++i)
            ::fmt::format_to(to_bytes + i * 2, ::fmt::runtime("{:02X}"), from_bytes[i]);
         return result;
      }

      /// The presence of this structure makes Bytes a CT::Serializer         
      struct CTTI_Serializer {
         /// The context holds the header entries, that allow us to           
         /// serialize types, tags, consts and verbs across sessions.         
         struct Context {
            template<class T>
            struct Bank {
               ::std::unordered_map<T, uint32_t> mDefinitions;
               uint32_t mNextId = 1;

               uint32_t Define(T&& meta) {
                  auto found = mDefinitions.find(meta);
                  if (found != mDefinitions.end())
                     return found->second;
                  mDefinitions[meta] = mNextId;
                  return mNextId++;
               }
            };

            Bank<RTTI::DMeta> mDMetaBank;
            Bank<RTTI::TMeta> mTMetaBank;
            Bank<RTTI::CMeta> mCMetaBank;
            Bank<RTTI::VMeta> mVMetaBank;
         };
         
         static constexpr bool CriticalFailure = true;
         static constexpr bool SkipElements = false;

         static void BeginScope(const CT::Container auto& from, Bytes& to, Context* context) {
            //TODO multidimensional containers like maps have multiple types
            if constexpr (requires { from.GetType(); }) {
               if (context) {
                  const auto typeId = context->mDMetaBank.Define(from.GetType());
                  to += typeId;
               }
               else to += from.GetType();
            }

            if constexpr (requires { from.GetState(); })
               to += from.GetState();

            if constexpr (requires { from.GetCount(); })
               to += from.GetCount();
         }
         
         static void EndScope(const CT::Container auto&, Text&, Context*) {
            // noop
         }
         
         static void Separate(const CT::Container auto&, Text&, Context*) {
            // noop
         }
         
         static void Empty(RTTI::DMeta type, CountType i, Text&, Context*) {
            LglsError("Item #", i, " of type `", type.GetName(),
               "` was serialized to an empty `Bytes`");
         }
         
         static void Error(RTTI::DMeta type, CountType i, Text&, Context*) {
            LglsError("Item #", i, " of type `", type.GetName(),
               "` failed to convert to `Bytes`");
         }
      };
   };
   #pragma pack(pop)
}

namespace Langulus::CTTI
{
   /// A rule for serializing any deep container.                             
   /// This includes Any, Many, Map, Set, Pair, Neat, Tag, etc...             
   /// as well as any templated equivalents.                                  
   template<CT::Deep C>
   struct SerializationRule<Anyness::Bytes, C> {
      using S = SerializerOf<Anyness::Bytes>;
      using Context = typename S::Context;
      using Count = Anyness::Bytes::CountType;
      
      static void Serialize(C const&, Anyness::Bytes&, Context*) requires CT::ContainsMany<C>;
      static void Serialize(C const&, Anyness::Bytes&, Context*) requires CT::ContainsOne<C>;
   };
   
   /// A rule for serializing meta data.                                      
   /// Will register it in the Context, and write it as an ID where needed.   
   template<>
   struct SerializationRule<Anyness::Bytes, RTTI::DMeta> {
      using S = SerializerOf<Anyness::Bytes>;
      using Context = typename S::Context;
      using Count = Anyness::Bytes::CountType;
      
      static void Serialize(RTTI::DMeta const&, Anyness::Bytes&, Context*);
   };
   
   /// A rule for serializing meta tags.                                      
   /// Will register it in the Context, and write it as an ID where needed.   
   template<>
   struct SerializationRule<Anyness::Bytes, RTTI::TMeta> {
      using S = SerializerOf<Anyness::Bytes>;
      using Context = typename S::Context;
      using Count = Anyness::Bytes::CountType;
      
      static void Serialize(RTTI::TMeta const&, Anyness::Bytes&, Context*);
   };
   
   /// A rule for serializing meta constants.                                 
   /// Will register it in the Context, and write it as an ID where needed.   
   template<>
   struct SerializationRule<Anyness::Bytes, RTTI::CMeta> {
      using S = SerializerOf<Anyness::Bytes>;
      using Context = typename S::Context;
      using Count = Anyness::Bytes::CountType;
      
      static void Serialize(RTTI::CMeta const&, Anyness::Bytes&, Context*);
   };
   
   /// A rule for serializing meta verbs.                                     
   /// Will register it in the Context, and write it as an ID where needed.   
   template<>
   struct SerializationRule<Anyness::Bytes, RTTI::VMeta> {
      using S = SerializerOf<Anyness::Bytes>;
      using Context = typename S::Context;
      using Count = Anyness::Bytes::CountType;
      
      static void Serialize(RTTI::VMeta const&, Anyness::Bytes&, Context*);
   };
}
