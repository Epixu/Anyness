///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/IndexedLinear.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Concatenate.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Emergent.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Conversion.hpp"
#include <Langulus/CT/Text.hpp>
#include <Langulus/CT/Number.hpp>
#include <Langulus/CT/Serializer.hpp>


namespace Langulus::Anyness
{
   struct Text;

   namespace Inner
   {
      using TextBase = Container<
         Com::TypedStatic<DMeta, char>,   // Type-constrained           
         Com::HeapMovable<>,              // Pointer to heap memory     
         Com::OwnershipStack<>,           // Allocation is referenced   
         Com::CountStack<>,               // Variable count             
         Com::ReserveEmergent<>,          // Capacity derived from alloc
         Com::HashStack<>,                // Variable hash (cached)     
         Com::Insertion<0, Text>,         // Serialize + insert         
         Com::InsertionOperators<0, Text>,// << and >> insertion        
         Com::Concatenate<>,              // Concatenate                
         Com::Removal<>,                  // Allows removal             
         Com::Assignment<>,               // Allows assignment          
         Com::Comparison<>,               // Allows for comparison      
         Com::Conversion,                 // Allows conversion          
         Com::IndexedLinear<>,            // Indexed directly           
         Com::IterationForEach<>,         // ForEach iteration          
         Com::IterationRange<>            // Range iteration            
      >;
   }


   ///                                                                        
   /// A continuous text container of variable size                           
   ///                                                                        
   struct Text : Inner::TextBase {
      using CountType = Base::CountType;
      using CTTI_Text = Yes<>;

      // Single element selections                                      
      using Pick    = char const&;
      using PickMut = char&;
      
      //using Base::operator =;
      //using Base::operator ==;

      constexpr Text() noexcept { this->ConstructDefault(); }
      constexpr Text(nullptr_t) noexcept    : Text {} {}
      constexpr Text(Text const& other)     : Text {Refer {other}} {}
      constexpr Text(Text&& other) noexcept : Text {Move  {other}} {}
      constexpr ~Text() noexcept { this->Destroy(); }

      /// Construction from any kind of text that is an Anyness container     
      template<CT::Text T> requires CT::Container<T>
      constexpr Text(T&& text) {
         this->ConstructFrom(FWD(text));
      }

      /// Construction from any kind of text that isn't an Anyness container  
      template<CT::Text T> requires CT::NotContainer<T>
      constexpr Text(T&& text) {
         using S  = IntentOf(text);
         using ST = TypeOf<S>;
         decltype(auto) source = DeintCast(FWD(text));

         if constexpr (CT::TextLiteral<ST>) {
            // Create from a text literal/bounded array                 
            // Type can be either char, or const char                   
            using CHAR = TypeOf<ST>;
            static_assert(Same<CHAR, char>, "Type mismatch");
            const auto count = ::std::char_traits<char>::length(source);
            if (not count) {
               this->ConstructDefault();
               return;
            }
            this->SetHeapInner(source);
            this->SetCountInner(count);
         }
         else if constexpr (CT::TextPointer<ST>) {
            // Create from a null-terminated char pointer               
            // Type can be either char, or const char                   
            if (not source) {
               this->ConstructDefault();
               return;
            }
            using CHAR = Deptr<ST>;
            static_assert(Same<CHAR, char>, "Type mismatch");
            const auto count = ::std::char_traits<char>::length(source);
            if (not count) {
               this->ConstructDefault();
               return;
            }
            this->SetHeapInner(source);
            this->SetCountInner(count);
         }
         else if constexpr (::std::ranges::contiguous_range<ST>) {
            // Create from an std container                             
            // Type can be either char, or const char                   
            if (source.empty()) {
               this->ConstructDefault();
               return;
            }
            using CHAR = Deptr<decltype(source.data())>;
            static_assert(Same<CHAR, char>, "Type mismatch");
            this->SetHeapInner(source.data());
            this->SetCountInner(source.size());
         }
         else static_assert(false, "Unsupported text constructor");

         // Reset hash                                                  
         this->ResetHash();

         // Take ownership if the intent requires it                    
         this->SetAllocationInner(nullptr);
         if constexpr (S::KeepsOnCopy())
            this->TakeOwnership();
      }

      /// Construction from all kinds of characters                           
      template<CT::Character T>
      constexpr Text(T&& ch) {
         this->AllocateFresh(this->RequestHeap(1));
         *this->GetRawAs<char>() = DeintCast(ch);
         this->SetCountInner(1);
         this->ResetHash();
      }
      
      /// Assignment                                                          
      constexpr Text& operator = (Text const& other) {
         return this->AssignFrom(Refer {other});
      }
      constexpr Text& operator = (Text&& other) noexcept {
         return this->AssignFrom(Move {other});
      }

      /// Construction from all kinds of text, trim length to desired count   
      ///   @attention intent is ignored - this doesn't apply ownership, only 
      ///      interfaces the data - you can TakeOwnership() after this call  
      ///   @attention count will shrink if a terminating character was found,
      ///      or if 'text' is a bounded array of smaller size                
      ///   @param text - text to wrap, assumed valid                         
      ///   @param count - number of characters inside 'text' to use          
      ///   @return the text wrapped inside a Text container                  
      template<CT::Text T>
      static Text FromText(T&& text, CountType count) {
         if (count == 0)
            return {};

         Text result {Disown {text}};
         if (count < result.GetCountInner())
            result.SetCountInner(count);
         return result;
      }
      
      /// Create text from a number                                           
      ///   @param number - the number to stringify                           
      ///   @param precision - number of digits after the floating point, use 
      ///      0 for no truncation. Will produce scientific notation for too  
      ///      big or too small numbers                                       
      ///   @return the text                                                  
      template<CT::Number T>
      static Text FromNumber(T&& number, int precision = 0) {
         Text result;

         if constexpr (CT::Real<T>) {
            // Stringify a real number                                  
            constexpr auto size = ::std::numeric_limits<T>::max_digits10 * 2;
            char temp[size];
            auto [lastChar, errorCode] = ::std::to_chars(
               temp, temp + size, number, ::std::chars_format::general);
            LglsAssert(errorCode == ::std::errc(), "std::to_chars failure");

            // Find the dot                                             
            auto dot = temp;
            while (dot < lastChar and *dot != '.')
               ++dot;

            if (dot == lastChar) {
               // There is no dot...                                    
               const auto c = static_cast<CountType>(lastChar - temp);
               result.AllocateFresh(result.RequestHeap(c));
               memcpy(result.GetHeapInner(), temp, c);
               result.SetCountInner(c);
               result.ResetHash();
               return result;
            }

            // Truncate or just remove all trailing zeroes back to dot  
            --lastChar;
            bool approximate = false;

            while (lastChar >= dot) {
               // If last digit is zero/dot directly skip it            
               if (*lastChar == '.' or *lastChar == '0') {
                  --lastChar;
                  continue;
               }

               if (precision) {
                  // We can truncate even more                          
                  if (lastChar > dot + precision) {
                     if (lastChar == dot + precision + 1 and *lastChar > '4') {
                        // Round up                                     
                        while (*lastChar == '9') {
                           // Propagate up until <9 or .                
                           --lastChar;
                        }

                        if (*lastChar == '.')
                           ++(*(--lastChar));
                        else
                           ++(*lastChar);
                     }
                     else --lastChar;

                     approximate = true;
                     continue;
                  }
               }
               break;
            }

            ++lastChar;
            const auto c = static_cast<CountType>(lastChar - temp);
            if (approximate) {
               // We've truncated the number, so prepend a '~' symbol   
               // to signify it's an approximate representation         
               result.AllocateFresh(result.RequestHeap(c + 1));
               auto heap = result.GetRawAs<char>();
               *heap = '~';
               memcpy(heap + 1, temp, c);
               result.SetCountInner(c + 1);
            }
            else {
               result.AllocateFresh(result.RequestHeap(c));
               memcpy(result.GetHeapInner(), temp, c);
               result.SetCountInner(c);
            }
         }
         else if constexpr (CT::Integer<T>) {
            // Stringify an integer                                     
            constexpr auto size = ::std::numeric_limits<T>::digits10 * 2;
            char temp[size];
            auto [lastChar, errorCode] = ::std::to_chars(temp, temp + size, number);
            LglsAssert(errorCode == ::std::errc(), "std::to_chars failure");

            const auto c = static_cast<CountType>(lastChar - temp);
            result.AllocateFresh(result.RequestHeap(c));
            memcpy(result.GetHeapInner(), temp, c);
            result.SetCountInner(c);
         }
         else static_assert(false, "Unsupported number type");

         result.ResetHash();
         return result;
      }

      /// Interpret text container as a std::string_view                      
      ///   @attention the string is null-terminated only after Terminate()   
      constexpr operator Token() const noexcept {
         return {this->GetRaw(), this->GetCount()};
      }

      /// Comparing against nullptr_t checks if text is empty                 
      constexpr bool operator == (nullptr_t) const noexcept {
         return this->IsEmpty();
      }

      /// Comparing against null-terminated strings                           
      constexpr bool operator == (const CT::TextPointer auto& rhs) const noexcept {
         if (rhs == nullptr or *rhs == 0)
            return this->IsEmpty();
         return operator == (Text {Disown(rhs)});
      }

      /// Comparing against std containers with characters                    
      constexpr bool operator == (const CT::TextRange auto& rhs) const noexcept {
         return operator == (Text {Disown(rhs)});
      }

      /// Comparison                                                          
      constexpr auto operator <=> (Text const& other) const noexcept -> ::std::partial_ordering {
         return this->Compare(other);
      }

      constexpr bool operator == (Text const& other) const noexcept {
         return this->CompareEqual(other);
      }

      /// Custom concatenation operator that includes string literals,        
      /// null-terminated string pointers, and std::continuous_ranges         
      ///   @note conventional container concatenation is defined in          
      ///      Com::Concatenate and Com::ConcatenateOperators                 
      template<CT::Text T> requires CT::NotContainer<T>
      Text& operator += (T&& rhs) {
         if (not this->IsAllocated()) {
            *this = Text {FWD(rhs)};
            return *this;
         }

         using DT = Deint<T>;
         decltype(auto) source = DeintCast(FWD(rhs));
         const auto currentCount = this->GetCount();

         if constexpr (CT::TextLiteral<DT>) {
            // Create from a text literal/bounded array                 
            using CHAR = TypeOf<DT>;
            static_assert(::std::same_as<Decvq<CHAR>, char>, "Type mismatch");
            const auto count = strnlen(source, ExtentOf<DT>);
            if (not count)
               return *this;
            this->AllocateMore(currentCount + count);
            memcpy(this->GetRawAs<uint8_t>() + currentCount, source, count);
            this->SetCountInner(currentCount + count);
         }
         else if constexpr (CT::TextPointer<DT>) {
            // Create from a null-terminated char pointer               
            if (not source)
               return *this;
            using CHAR = Deptr<DT>;
            static_assert(::std::same_as<Decvq<CHAR>, char>, "Type mismatch");
            const auto count = strlen(source);
            if (not count)
               return *this;
            this->AllocateMore(currentCount + count);
            memcpy(this->GetRawAs<uint8_t>() + currentCount, source, count);
            this->SetCountInner(currentCount + count);
         }
         else if constexpr (::std::ranges::contiguous_range<DT>) {
            // Create from an std container                             
            if (source.empty())
               return *this;
            using CHAR = Deptr<decltype(source.data())>;
            static_assert(::std::same_as<Decvq<CHAR>, char>, "Type mismatch");
            const auto count = source.size();
            this->AllocateMore(currentCount + count);
            memcpy(this->GetRawAs<uint8_t>() + currentCount, source.data(), count);
            this->SetCountInner(currentCount + count);
         }
         else static_assert(false, "Unsupported text concatenation");

         this->ResetHash();
         return *this;
      }
   };

   struct Code : Text {};
   struct Bytes;
   
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Do types have an explicit/implicit cast operator to Text            
      template<class...T>
      concept StringifiableByOperator = (std::is_object_v<T> and ...)
          and requires (const T&...a) {
            ((a.operator ::Langulus::Anyness::Text()), ...);
         };

      /// Does Text has an explicit/implicit constructor that accepts T       
      template<class...T>
      concept StringifiableByConstructor = requires (const T&...a) {
         ((::Langulus::Anyness::Text {a}), ...); };
   }

   /// A stringifiable type is one that has either an implicit or explicit    
   /// cast operator to Text type, or can be used to explicitly initialize a  
   /// Text container                                                         
   template<class...T>
   concept Stringifiable = ((Inner::StringifiableByOperator<T>
                          or Inner::StringifiableByConstructor<T>) and ...);
}

namespace Langulus
{
   /// Make a text literal                                                    
   inline Anyness::Text operator ""_text(const char* text, size_t size) {
      return Anyness::Text::FromText(text, size);
   }
}

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
      
      //using Operator = Serial::Operator;

      //static constexpr auto Operators = Serial::Operators;
      static constexpr bool CriticalFailure = false;
      static constexpr bool SkipElements = true;

      static void BeginScope(const CT::Container auto& from, Text& to, Context*) {
         const bool scoped = from.GetCount() > 1 or from.IsInvalid() or from.IsExecutable(); //TODO could carry in context and check verb precedence to avoid scoping in some cases
         if (scoped) {
            if (from.IsPast())
               to += Operator::Past;
            else if (from.IsFuture())
               to += Operator::Future;

            to += Operator::OpenScope;
         }
      }
      
      static void EndScope(const CT::Container auto& from, Text& to, Context*) {
         const bool scoped = from.GetCount() > 1 or from.IsInvalid() or from.IsExecutable(); //TODO could carry in context and check verb precedence to avoid scoping in some cases
         if (scoped)
            to += Operator::CloseScope;
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
      
      static void Serialize(C const& self, Anyness::Text& out, Context* context) {
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Serialize a type-erased container                        
            const auto T = self.GetType();
            if (T.IsDeep()) {
               for (Count i = 0; i < self.GetCount(); ++i) {
                  auto item = self.template CastAt<typename C::DeepType>(i);
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
               auto serializer = T.GetMorphism(MetaDataOf<Text>()).serialize;
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
   };

   /// Rule for serializing Code to Text. Wraps it in {} symbols.             
   template<>
   struct SerializationRule<Anyness::Text, Anyness::Code> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static void Serialize(const Anyness::Code& item, Anyness::Text& out, Context* context) {
         out += Operator::OpenCode;
         out += static_cast<Anyness::Text>(item);
         out += Operator::CloseCode;
      }
   };
   
   /// Rule for serializing Text to Text. Wraps it in "".                     
   template<>
   struct SerializationRule<Anyness::Text, Anyness::Text> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static auto Serialize(const Anyness::Text& item, Anyness::Text& out, Context* context) {
         out += Operator::OpenString;
         out += item;
         out += Operator::CloseString;
      }
   };
   
   /// Rule for serializing Bytes to Text. Prepends 0x.                       
   template<>
   struct SerializationRule<Anyness::Text, Anyness::Bytes> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static auto Serialize(const Anyness::Bytes& item, Anyness::Text& out, Context* context) {
         out += Operator::OpenByte;
         out += static_cast<Anyness::Text>(item);
      }
   };
   
   /// Rule for serializing characters to Text. Wraps them in ''.             
   template<CT::Character C>
   struct SerializationRule<Anyness::Text, C> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static auto Serialize(C const& item, Anyness::Text& out, Context* context) {
         out += Operator::OpenCharacter;
         out += static_cast<Anyness::Text>(item);
         out += Operator::CloseCharacter;
      }
   };
}
