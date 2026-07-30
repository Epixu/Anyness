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
   /// A contiguous byte container of variable size                           
   ///                                                                        
   #pragma pack(push, 4)
   struct Bytes : Inner::BytesBase {
      using CTTI_ReflectAs = Bytes;
      using CTTI_MapsTo    = Types<Bytes, Text>;
      using CTTI_MapsFrom  = Types<
         bool, char, /*wchar_t, char8_t, char16_t, char32_t,*/
         int8_t, int16_t, int32_t, int64_t,
         uint8_t, uint16_t, uint32_t, uint64_t,
         float, double,
         Hash, Byte,
         RTTI::DMeta, RTTI::TMeta, RTTI::CMeta, RTTI::VMeta
      >;

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
      
      /// Construction from raw bytes data                                    
      ///   @param data data to wrap, assumed valid                           
      ///   @param count number of bytes inside 'data' to use                 
      ///   @return the raw bytes wrapped inside a Bytes container            
      static Bytes FromBytes(void const* data, size_t count) {
         if (count == 0)
            return {};

         Bytes result;
         result.EnableDisowned();
         result.SetHeapInner(data);
         result.SetCountInner(count);
         return result;
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
            enum class BuiltInTypes {
               Bool = 1,
               I8, I16, I32, I64,
               U8, U16, U32, U64,
               Char, Byte, Half, Float, Double,
               _Counter_
            };

            template<class T>
            struct Bank {
               ::std::unordered_map<T, uint64_t> mDefinitions;
               uint64_t mNextId = Same<T, RTTI::DMeta> ? static_cast<uint64_t>(BuiltInTypes::_Counter_) : 1;

               uint64_t Define(T&& meta) {
                  // Built-in types are reserved, no need to serialize  
                  // them.                                              
                  if (not meta)
                     return 0;
                  
                  if constexpr (Same<T, RTTI::DMeta>) {
                     if (meta.IsSame(MetaDataOf<bool>()))
                        return static_cast<uint64_t>(BuiltInTypes::Bool);
                     else if (meta.IsSame(MetaDataOf<int8_t>()))
                        return static_cast<uint64_t>(BuiltInTypes::I8);
                     else if (meta.IsSame(MetaDataOf<int16_t>()))
                        return static_cast<uint64_t>(BuiltInTypes::I16);
                     else if (meta.IsSame(MetaDataOf<int32_t>()))
                        return static_cast<uint64_t>(BuiltInTypes::I32);
                     else if (meta.IsSame(MetaDataOf<int64_t>()))
                        return static_cast<uint64_t>(BuiltInTypes::I64);
                     else if (meta.IsSame(MetaDataOf<uint8_t>()))
                        return static_cast<uint64_t>(BuiltInTypes::U8);
                     else if (meta.IsSame(MetaDataOf<uint16_t>()))
                        return static_cast<uint64_t>(BuiltInTypes::U16);
                     else if (meta.IsSame(MetaDataOf<uint32_t>()))
                        return static_cast<uint64_t>(BuiltInTypes::U32);
                     else if (meta.IsSame(MetaDataOf<uint64_t>()))
                        return static_cast<uint64_t>(BuiltInTypes::U64);
                     else if (meta.IsSame(MetaDataOf<char>()))
                        return static_cast<uint64_t>(BuiltInTypes::Char);
                     else if (meta.IsSame(MetaDataOf<Byte>()))
                        return static_cast<uint64_t>(BuiltInTypes::Byte);
                     //else if (meta.IsSame(MetaDataOf<half>())) //TODO
                     //   return static_cast<uint64_t>(BuiltInTypes::Half;
                     else if (meta.IsSame(MetaDataOf<float>()))
                        return static_cast<uint64_t>(BuiltInTypes::Float);
                     else if (meta.IsSame(MetaDataOf<double>()))
                        return static_cast<uint64_t>(BuiltInTypes::Double);
                  }

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

         /// Flags used in the headbit                                        
         enum Headbits : uint8_t {
            // Skips an element, default initializes it if needed       
            Skip = 0,

            // Subsequent data is big endian                            
            BigEndian = 1,

            // A type ID is serialized for the next element, for each   
            // individual dimension. The size of the ID starts at 8bit  
            // and increases using Large16/Large32 flags.               
            // The ID indexes a definition in the context, so this      
            // is usually accompanied with the HasDependencies flag.    
            Typed = 2,

            // A 8bit state is serialized for the next element.         
            // If accompanied with Large16/Large32, the size increases. 
            Stateful = 4,

            // Serializes count, assumed 1 if this flag is missing.     
            // When Large16/Large32 are enabled, the size of the        
            // counter increases.                                       
            Multiple = 8,

            // Signifies that state and count get more bits in order    
            // to serialize more elements.                              
            Large16 = 16,

            // Signifies that state and count get even more bits in     
            // order to serialize even more elements. Can be combined   
            // with Large16 in order to jump up to 64 bits.             
            Large32 = 32,

            Large64 = Large16 | Large32,

            // Signifies that the serialized data uses the context.     
            HasDependencies = 64,

            // Serializes the number of dimensions, otherwise assumed 1.
            // Always of size 1 byte, for up to 256 dimensions.         
            Multidimensional = 128
         };

         /// Serializes any container                                         
         ///   @param from the container to serialize                         
         ///   @param to where serialized bytes get appended                  
         ///   @param context optional context for storing repeating patterns 
         template<CT::Container C>
         static void BeginScope(C const& from, Bytes& to, Context* context) {
            if (not from.IsValid()) {
               to += Headbits::Skip;
               return;
            }

            // Write header                                             
            /// @attention this is the biggest possible header size,    
            ///    but this one in particular doesn't allocate space for
            ///    more than two types!                                 
            constexpr size_t max_dimensions = 2;
            uint8_t header[1 + 1 + max_dimensions*8 + 8 + 8];
            uint8_t& headbyte = header[0];
            size_t progress = 1;

            if (std::endian::native == std::endian::big)
               headbyte |= Headbits::BigEndian;

            if (C::Dimensions::Count > 1) {
               static_assert(C::Dimensions::Count <= max_dimensions,
                  "Update max_dimensions for more dimensions. "
                  "This is not set to max to save on stack memory"
               );
               headbyte |= Headbits::Multidimensional;
               header[progress] = static_cast<uint8_t>(C::Dimensions::Count);
               ++progress;
            }

            // First pass goes through all variable-sized counters and  
            // populates the header flags before writing anything.      
            if constexpr (requires { from.GetType(); }) {
               C::Dimensions::ForEach([&]<Cid D> {
                  LglsAssert(context,
                     "Context is required for binary serialization of containers");

                  headbyte |= Headbits::Typed;

                  const uint64_t typeId = context->mDMetaBank.Define(from.template GetType<D>());
                  if (typeId >= static_cast<uint64_t>(Context::BuiltInTypes::_Counter_))
                     headbyte |= Headbits::HasDependencies;

                  if (typeId < 256)
                     ;
                  else if (typeId < 65536)
                     headbyte |= Headbits::Large16;
                  else if (typeId < 4294967296)
                     headbyte |= Headbits::Large32;
                  else
                     headbyte |= Headbits::Large64;
               });
            }

            if constexpr (requires { from.GetUnconstrainedState(); }) {
               const uint64_t s = from.GetUnconstrainedState();
               if (s != 0) {
                  headbyte |= Headbits::Stateful;

                  if (s < 256)
                     ;
                  else if (s < 65536)
                     headbyte |= Headbits::Large16;
                  else if (s < 4294967296)
                     headbyte |= Headbits::Large32;
                  else
                     headbyte |= Headbits::Large64;
               }
            }

            if constexpr (requires { from.GetCount(); }) {
               const uint64_t s = from.GetCount();
               if (s != 1) {
                  headbyte |= Headbits::Multiple;

                  if (s < 256)
                     ;
                  else if (s < 65536)
                     headbyte |= Headbits::Large16;
                  else if (s < 4294967296)
                     headbyte |= Headbits::Large32;
                  else
                     headbyte |= Headbits::Large64;
               }
            }

            // Now write the data                                       
            if constexpr (requires { from.GetType(); }) {
               C::Dimensions::ForEach([&]<Cid D> {
                  const uint64_t typeId = context->mDMetaBank.Define(from.template GetType<D>());
                  if ((headbyte & Headbits::Large64) == Headbits::Large64) {
                     memcpy(header + progress, &typeId, 8);
                     progress += 8;
                  }
                  else if (headbyte & Headbits::Large32) {
                     const uint32_t typeId32 = static_cast<uint32_t>(typeId);
                     memcpy(header + progress, &typeId32, 4);
                     progress += 4;
                  }
                  else if (headbyte & Headbits::Large16) {
                     const uint16_t typeId16 = static_cast<uint16_t>(typeId);
                     memcpy(header + progress, &typeId16, 2);
                     progress += 2;
                  }
                  else {
                     header[progress] = static_cast<uint8_t>(typeId);
                     ++progress;
                  }
               });
            }

            if constexpr (requires { from.GetUnconstrainedState(); }) {
               const uint64_t s = from.GetUnconstrainedState();
               if ((headbyte & Headbits::Large64) == Headbits::Large64) {
                  memcpy(header + progress, &s, 8);
                  progress += 8;
               }
               else if (headbyte & Headbits::Large32) {
                  const uint32_t s32 = static_cast<uint32_t>(s);
                  memcpy(header + progress, &s32, 4);
                  progress += 4;
               }
               else if (headbyte & Headbits::Large16) {
                  const uint16_t s16 = static_cast<uint16_t>(s);
                  memcpy(header + progress, &s16, 2);
                  progress += 2;
               }
               else {
                  header[progress] = static_cast<uint8_t>(s);
                  ++progress;
               }
            }

            if constexpr (requires { from.GetCount(); }) {
               const uint64_t s = from.GetCount();
               if ((headbyte & Headbits::Large64) == Headbits::Large64) {
                  memcpy(header + progress, &s, 8);
                  progress += 8;
               }
               else if (headbyte & Headbits::Large32) {
                  const uint32_t s32 = static_cast<uint32_t>(s);
                  memcpy(header + progress, &s32, 4);
                  progress += 4;
               }
               else if (headbyte & Headbits::Large16) {
                  const uint16_t s16 = static_cast<uint16_t>(s);
                  memcpy(header + progress, &s16, 2);
                  progress += 2;
               }
               else {
                  header[progress] = static_cast<uint8_t>(s);
                  ++progress;
               }
            }

            // Finally, write the entire header to the stream           
            to += Bytes::FromBytes(header, progress);
         }
         
         static void EndScope(const CT::Container auto&, Bytes&, Context*) {
            // noop
         }
         
         static void Separate(const CT::Container auto&, Bytes&, Context*) {
            // noop
         }
         
         static void Empty(RTTI::DMeta type, CountType i, Bytes&, Context*) {
            LglsError("Item #", i, " of type `", type.GetName(),
               "` was serialized to an empty `Bytes`");
         }
         
         static void Error(RTTI::DMeta type, CountType i, Bytes&, Context*) {
            LglsError("Item #", i, " of type `", type.GetName(),
               "` failed to convert to `Bytes`");
         }
      };
   };
   #pragma pack(pop)
}

namespace Langulus::CTTI
{
   /// A rule for serializing any deep container, regardless of sparsity.     
   /// This includes Any, Many, Map, Set, Pair, Neat, Tag, etc...             
   /// as well as any templated equivalents. Adds a header for each nested.   
   /// deep container.                                                        
   /*template<CT::Deep C>
   struct SerializationRule<Anyness::Bytes, C> {
      static_assert(Exact<DecvqAll<C>, C>,
         "Strip all decorations on all indirections first");

      using S = SerializerOf<Anyness::Bytes>;
      using Context = typename S::Context;
      using Count = Anyness::Bytes::CountType;
      
      static void Serialize(ConstAll<C&>, Anyness::Bytes&, Context*) requires CT::ContainsMany<Decay<C>>;
      //static void Serialize(ConstAll<C&>, Anyness::Bytes&, Context*) requires CT::ContainsOne<Decay<C>>;
   };*/

   /// Rule for serializing any container that isn't deep.                    
   template<class C> requires CT::Container<Decay<C>>
   struct SerializationRule<Anyness::Bytes, C> {
      static_assert(Exact<DecvqAll<C>, C>,
         "Strip all decorations on all indirections first");

      using S = SerializerOf<Anyness::Bytes>;
      using Context = typename S::Context;

      static void Serialize(ConstAll<C&>, Anyness::Bytes&, Context*);
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
