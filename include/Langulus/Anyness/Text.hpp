///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Handle.hpp"
//#include "Langulus/Typenav.hpp"
#include <source/components/Heap-Movable.hpp>
#include <source/components/Ownership-Stack.hpp>
#include <source/components/IndexedLinear.hpp>
#include <source/components/Insertion.hpp>
#include <source/components/InsertionOperators.hpp>
#include <source/components/InsertionOperatorsConcat.hpp>
#include <source/components/Merging.hpp>
#include <source/components/MergingOperators.hpp>
#include <source/components/Removal.hpp>
#include <source/components/Assignment.hpp>
#include <source/components/Typed-Static.hpp>
#include <source/components/Count-Stack.hpp>
#include <source/components/Reserve-Emergent.hpp>
#include <source/components/Hash-Stack.hpp>
#include <source/components/Iteration-ForEach.hpp>
#include <source/components/Iteration-Range.hpp>
#include <source/components/Comparison.hpp>
#include <source/components/Conversion.hpp>
#include <source/states/Disowned.hpp>
#include <source/states/Compressed.hpp>
#include <source/states/Encrypted.hpp>
#include <Langulus/CT/Text.hpp>
#include <Langulus/CT/Number.hpp>
#include <Langulus/CT/Convertible.hpp>
#include <Langulus/CT/Serializer.hpp>
#include <Langulus/Utils/Byte.hpp>
//#include <string_view>
//#include <type_traits>


namespace Langulus::Anyness
{
   struct Text;
   struct Bytes;

   namespace Inner
   {
      using TextBase = Com::Container<
         Com::State::Disowned<>,          // Allows disownment          
         Com::TypedStatic<DMeta, char>,   // Type-constrained           
         Com::HeapMovable<0, 0, HeapEntry<0, char*>>,
         Com::CountStack<>,               // Variable count             
         Com::ReserveEmergent<>,          // Capacity derived from alloc
         Com::IndexedLinear<>,            // Indexed directly           
         Com::OwnershipStack<>,           // Allocation is referenced   
         Com::HashStack<>,                // Variable hash (cached)     
         Com::Insertion<true>,            // Serialize + insert         
         Com::InsertionOperators<>,       // << and >> insertion        
         Com::InsertionOperatorsConcat<>, // + and += concat            
         Com::Merging<true>,              // Serialize + merge          
         Com::MergingOperators<>,         // <<= and >>= merging        
         Com::Removal<>,                  // Allows removal             
         Com::Assignment<true>,           // Allows assignment          
         Com::Comparison<true>,           // Allows for comparison      
         Com::Conversion<>,               // Allows conversion          
         Com::IterationForEach<>,         // ForEach iteration          
         Com::IterationRange<>,           // Range iteration            
         Com::State::Compressed<>,        // Toggle compression         
         Com::State::Encrypted<>          // Toggle encryption          
      >;
   }


   ///                                                                        
   /// A continuous text container of variable size                           
   ///                                                                        
   struct Text : Inner::TextBase {
      using CTTI_ReflectAs = Text;
      using CTTI_Text      = Yes<>;

      using CountType = Base::CountType;

      constexpr Text() noexcept {
         this->ConstructDefault();
      }

      constexpr Text(nullptr_t) noexcept
         : Text {} {}

      constexpr Text(Text const& other)
         : Text {Refer {other}} {}

      constexpr Text(Text&& other) noexcept
         : Text {Move  {other}} {}

      constexpr ~Text() noexcept {
         this->Destroy();
      }

      /// Construction that absorbs the provided containers                   
      template<class A1, class...AN>
      constexpr Text(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Concat(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr Text(Inner::Piecewise, A1&& a1, AN&&...an) {
         this->ConstructDefault();
         this->Insert(LglsFwd(a1), LglsFwd(an)...);
      }

      /// Construction from Serial::Operator                                  
      ///   @attention this is a non-owning constructor, often used as a      
      ///      temporary                                                      
      explicit constexpr Text(Serial::Operator const& o)
         : Text {o.mToken} {}

      /// Construction from any kind of text that is an Anyness container     
      template<CT::Text T> requires CT::Container<T>
      constexpr Text(T&& text) {
         this->Absorb(LglsFwd(text));
      }

      /// Construction from any kind of text that isn't an Anyness container  
      template<CT::Text T> requires CT::NotContainer<T>
      constexpr Text(T&& text) {
         using I  = IntentOf(text);
         using IT = TypeOf<I>;
         decltype(auto) source = DeintCast(LglsFwd(text));

         this->ResetState();

         if constexpr (CT::TextLiteral<IT>) {
            // Create from a text literal/bounded array                 
            using CHAR = TypeOf<IT>;
            static_assert(Same<CHAR, char>, "Type mismatch");

            CHAR const* src = source;
            CHAR const* const srcEnd = src + ExtentOf<IT>;
            while (src < srcEnd and *src)
               ++src;

            const auto count = src - source;
            if (not count) {
               this->ConstructDefault();
               return;
            }

            this->SetHeapInner(source);
            this->SetCountInner(count);

            // Bounded arrays and literals are always considered        
            // constexpr, thus no point in searching for their managed  
            // memory.                                                  
            this->SetAllocationInner(nullptr);
         }
         else if constexpr (CT::TextPointer<IT>) {
            // Create from a null-terminated char pointer               
            if (not source) {
               this->ConstructDefault();
               return;
            }

            using CHAR = Deptr<IT>;
            static_assert(Same<CHAR, char>, "Type mismatch");

            size_t count;
            if constexpr (CT::CustomPointer<decltype(source)>)
               count = ::std::char_traits<char>::length(source.Unpack());
            else
               count = ::std::char_traits<char>::length(source);
            
            if (not count) {
               this->ConstructDefault();
               return;
            }
            this->SetHeapInner(source);
            this->SetCountInner(count);

            // We may own this pointer                                  
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               if constexpr (CT::Disowned<I> or CT::Copied<I> or CT::Cloned<I>)
                  this->SetAllocationInner(nullptr);
               else
                  this->FindAllocationInner();
            #else
               this->SetAllocationInner(nullptr);
            #endif
         }
         else {
            // Create from an std container                             
            static_assert(::std::ranges::contiguous_range<IT>,
               "Unsupported text constructor");

            if (source.empty()) {
               this->ConstructDefault();
               return;
            }

            using CHAR = Deptr<decltype(source.data())>;
            static_assert(Same<CHAR, char>, "Type mismatch");
            this->SetHeapInner(source.data());
            this->SetCountInner(source.size());

            // Assumed never owned by us, no point in searching for the 
            // allocation.                                              
            this->SetAllocationInner(nullptr);
         }

         this->ResetHash();

         // Take ownership if the intent requires it                    
         if constexpr (CT::Copied<I> or CT::Cloned<I>)
            this->TakeOwnership();
      }

      /// Construction from all kinds of characters                           
      ///   @attention this is an owning constructor                          
      template<CT::Character T>
      constexpr Text(T&& ch) {
         this->ResetState();
         this->AllocateFresh(1);
         *this->GetRawAs<char>() = DeintCast(ch);
         this->SetCountInner(1);
         this->ResetHash();
      }
      
      /// MARK: =                                                             
      constexpr Text& operator = (Text const& other) {
         return this->AssignAbsorb(Refer {other});
      }
      constexpr Text& operator = (Text&& other) noexcept {
         return this->AssignAbsorb(Move {other});
      }

      template<class A>
      constexpr Text& operator = (A&& argument) {
         if constexpr (CT::Text<A> and CT::Container<A>)
            return this->AssignAbsorb(LglsFwd(argument));
         else
            return this->Assign(LglsFwd(argument));
      }
      
      /// Construction from all kinds of text, trim length to desired count   
      ///   @attention intent is ignored - this doesn't apply ownership, only 
      ///      interfaces the data - you can TakeOwnership() after this call  
      ///   @attention count will shrink if a terminating character was found,
      ///      or if 'text' is a bounded array of smaller size                
      ///   @param text text to wrap, assumed valid                           
      ///   @param count number of characters inside 'text' to use            
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
      ///   @param number the number to stringify                             
      ///   @param precision number of digits after the floating point, use   
      ///      0 for no truncation. Will produce scientific notation for too  
      ///      big or too small numbers                                       
      ///   @return the text                                                  
      template<CT::Number T>
      static Text FromNumber(T&& number, int precision = 0) {
         Text result;
         using DT = Decay<T>;

         if constexpr (CT::Real<T>) {
            // Stringify a real number                                  
            constexpr auto size = ::std::numeric_limits<DT>::max_digits10 * 2;
            char temp[size];
            auto [lastChar, errorCode] = ::std::to_chars(
               temp, temp + size, number, ::std::chars_format::general
            );
            LglsAssert(errorCode == ::std::errc(), "std::to_chars failure");

            // Find the dot                                             
            auto dot = temp;
            while (dot < lastChar and *dot != '.')
               ++dot;

            if (dot == lastChar) {
               // There is no dot...                                    
               const auto c = static_cast<CountType>(lastChar - temp);
               result.AllocateFresh(c /*result.RequestHeap(c)*/);
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
               result.AllocateFresh(c + 1);
               auto heap = result.GetRawAs<char>();
               *heap = '~';
               memcpy(heap + 1, temp, c);
               result.SetCountInner(c + 1);
            }
            else {
               result.AllocateFresh(c);
               memcpy(result.GetHeapInner(), temp, c);
               result.SetCountInner(c);
            }
         }
         else if constexpr (CT::Integer<T>) {
            // Stringify an integer                                     
            constexpr auto size = ::std::numeric_limits<DT>::digits10 * 2;
            char temp[size];
            auto [lastChar, errorCode] = ::std::to_chars(temp, temp + size, number);
            LglsAssert(errorCode == ::std::errc(), "std::to_chars failure");

            const auto c = static_cast<CountType>(lastChar - temp);
            result.AllocateFresh(c /*result.RequestHeap(c)*/);
            memcpy(result.GetHeapInner(), temp, c);
            result.SetCountInner(c);
         }
         else static_assert(false, "Unsupported number type");

         result.ResetHash();
         return result;
      }

      /// Generate hexadecimal string from a given value                      
      ///   @param from - the argument                                        
      ///   @return the resulting text                                        
      template<bool REVERSE = false>
      static Text Hex(const auto& from) {
         Text result;
         result.AllocateFresh(sizeof(from) * 2);
         auto from_bytes = reinterpret_cast<const char*>(&from);
         auto to_bytes = result.GetRaw();
         for (size_t i = 0; i < sizeof(from); ++i) {
            if constexpr (REVERSE)
               ::fmt::format_to_n(to_bytes + i * 2, 2, "{:02X}", from_bytes[sizeof(from) - (i + 1)]);
            else
               ::fmt::format_to_n(to_bytes + i * 2, 2, "{:02X}", from_bytes[i]);
         }
         result.SetCountInner(sizeof(from) * 2);
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
      constexpr auto operator <=> (CT::TextRange auto const& other) const noexcept -> ::std::partial_ordering {
         return this->Compare(other);
      }

      constexpr auto operator <=> (Text const& other) const noexcept -> ::std::partial_ordering {
         return this->Compare(other);
      }

      constexpr bool operator == (Text const& other) const noexcept {
         return this->CompareEqual(other);
      }

      explicit operator ::std::string() const {
         return {this->GetRaw(), this->GetCount()};
      }

      /// The presence of this structure makes Text a CT::Serializer          
      struct CTTI_Serializer {
         // Text serializer can be lossy to omit unnecessary details,   
         // and you can configure how many elements to show by defining 
         // LANGULUS_MAX_DEBUGGABLE_ELEMENTS.                           
         #ifdef LANGULUS_MAX_DEBUGGABLE_ELEMENTS
            static constexpr CountType MaxIterations = LANGULUS_MAX_DEBUGGABLE_ELEMENTS;
         #elif LANGULUS(DEBUG) or LANGULUS(SAFE)
            static constexpr CountType MaxIterations = 32;
         #else
            static constexpr CountType MaxIterations = 8;
         #endif

         struct Context {};
         
         static constexpr bool CriticalFailure = false;
         static constexpr bool SkipElements = true;

         static void BeginScope(const CT::Container auto& from, Text& to, Context*) {
            //TODO multidimensional containers like maps have multiple types
            const bool scoped = from.GetCount() > 1 or not from.IsValid() or from.IsExecutable(); //TODO could carry in context and check verb precedence to avoid scoping in some cases
            if (scoped)
               to += Serial::OpenScope;
         }
         
         static void EndScope(const CT::Container auto& from, Text& to, Context*) {
            //TODO multidimensional containers like maps have multiple types
            const bool scoped = from.GetCount() > 1 or not from.IsValid() or from.IsExecutable(); //TODO could carry in context and check verb precedence to avoid scoping in some cases
            if (scoped)
               to += Serial::CloseScope;
         }
         
         static void Separate(const CT::Container auto& from, Text& to, Context*) {
            if constexpr (requires { from.IsOrdered(); }) {
               if constexpr (requires { from.IsOr(); })
                  to += (from.IsOr() ? " or " : (from.IsOrdered() ? ", " : "; "));
               else
                  to += (from.IsOrdered() ? ", " : "; ");
            }
            else if constexpr (requires { from.IsOr(); })
               to += (from.IsOr() ? " or " : ", ");
            else 
               to += ", ";
         }
         
         static void Empty(RTTI::DMeta type, CountType i, Text& to, Context*) {
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
         
         static void Error(RTTI::DMeta type, CountType i, Text& to, Context*) {
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
   };

   struct Code : Text {};
   
   inline Text operator ""_text(const char* token, size_t size) noexcept {
      return Text::FromText(token, size);
   }
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

namespace Langulus::CTTI
{
   /// A rule for serializing any deep container, regardless of sparsity.     
   /// This includes Any, Many, Map, Set, Pair, Neat, Tag, etc...             
   /// as well as any templated equivalents. It basically places scopes,      
   /// separators and state decorators, depending on the kind of container.   
   template<CT::Deep C>
   struct SerializationRule<Anyness::Text, C> {
      static_assert(Exact<DecvqAll<C>, C>,
         "Strip all decorations on all indirections first");

      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;
      using Count = Anyness::Text::CountType;
      
      static void Serialize(ConstAll<C&>, Anyness::Text&, Context*) requires CT::ContainsMany<Decay<C>>;
      static void Serialize(ConstAll<C&>, Anyness::Text&, Context*) requires CT::ContainsOne<Decay<C>>;
   };

   /// Rule for serializing Code to Text. Wraps it in {} symbols.             
   template<CT::Container C> requires (not CT::Deep<C>)
   struct SerializationRule<Anyness::Text, C> {
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static void Serialize(ConstAll<C&>, Anyness::Text&, Context*);
   };
   
   /// Rule for serializing characters to Text. Wraps them in ''.             
   template<CT::Character C>
   struct SerializationRule<Anyness::Text, C> {
      static_assert(CT::Decayed<C>, "Strip all decorations first");
      using S = SerializerOf<Anyness::Text>;
      using Context = typename S::Context;

      static void Serialize(C const&, Anyness::Text&, Context*);
   };
   
   /// Map all pointers as convertible to text                                
   template<CT::Sparse T>
   struct ConverterFrom<T, LglsUniqueConverterIndex(T)> {
      LANGULUS_MORPHISM(Anyness::Text);
      static_assert(Exact<DecvqAll<T>, T>,
         "Strip all decorations on all indirections first");

      template<class TO>
      static constexpr TO Convert(ConstAll<T&> from) {
         if constexpr (CT::Complete<Deptr<T>>) {
            if constexpr (CT::Character<Deptr<T>>)
               return {from};
            else
               return NameOf<T>() + "(" + Anyness::Text::Hex<true>(from) + ")";
         }
         else return NameOf<T>() + "(" + Anyness::Text::Hex<true>(from) + ")";
      }
   };

   /// Convert std::string_view -> Text                                       
   template<>
   struct ConverterFrom<::std::string_view, LglsUniqueConverterIndex(::std::string_view)> {
      LANGULUS_MORPHISM(Anyness::Text);
   };

   /// Convert Serial::Operator -> Text                                       
   template<>
   struct ConverterFrom<Serial::Operator, LglsUniqueConverterIndex(Serial::Operator)> {
      LANGULUS_MORPHISM(Anyness::Text);
   };

   /// Convert Bool -> Text                                                   
   template<>
   struct ConverterFrom<bool, LglsUniqueConverterIndex(bool)> {
      LANGULUS_MORPHISM(Anyness::Text);

      template<class TO>
      static constexpr TO Convert(bool const& from) noexcept {
         return from ? "yes" : "no";
      }
   };

   /// Convert Byte -> Text                                                   
   template<>
   struct ConverterFrom<Langulus::Byte, LglsUniqueConverterIndex(Langulus::Byte)> {
      LANGULUS_MORPHISM(Anyness::Text);

      template<class TO>
      static constexpr TO Convert(Langulus::Byte const& from) noexcept {
         return Anyness::Text::Hex(from);
      }
   };

   /// Convert Hash -> Text                                                   
   template<>
   struct ConverterFrom<Langulus::Hash, LglsUniqueConverterIndex(Langulus::Hash)> {
      LANGULUS_MORPHISM(Anyness::Text);

      template<class TO>
      static constexpr TO Convert(Langulus::Hash const& from) noexcept {
         return Anyness::Text::Hex(from.value);
      }
   };

   /// Convert Number -> Text                                                 
   template<CT::Number T>
   struct ConverterFrom<T, LglsUniqueConverterIndex(T)> {
      static_assert(CT::Decayed<T>, "Strip all decorations first");
      LANGULUS_MORPHISM(Anyness::Text);

      template<class TO>
      static constexpr TO Convert(T const& from) noexcept {
         return Anyness::Text::FromNumber(from);
      }
   };

   /// Convert DMeta -> Text                                                  
   template<>
   struct ConverterFrom<RTTI::DMeta, LglsUniqueConverterIndex(RTTI::DMeta)> {
      LANGULUS_MORPHISM(Anyness::Text);

      template<class TO>
      static constexpr TO Convert(RTTI::DMeta const& from) noexcept {
         return from.GetName();
      }
   };

   /// Convert TMeta -> Text                                                  
   template<>
   struct ConverterFrom<RTTI::TMeta, LglsUniqueConverterIndex(RTTI::TMeta)> {
      LANGULUS_MORPHISM(Anyness::Text);

      template<class TO>
      static constexpr TO Convert(RTTI::TMeta const& from) noexcept {
         return from.GetName();
      }
   };

   /// Convert CMeta -> Text                                                  
   template<>
   struct ConverterFrom<RTTI::CMeta, LglsUniqueConverterIndex(RTTI::CMeta)> {
      LANGULUS_MORPHISM(Anyness::Text);

      template<class TO>
      static constexpr TO Convert(RTTI::CMeta const& from) noexcept {
         return from.GetName();
      }
   };

   /// Convert VMeta -> Text                                                  
   template<>
   struct ConverterFrom<RTTI::VMeta, LglsUniqueConverterIndex(RTTI::VMeta)> {
      LANGULUS_MORPHISM(Anyness::Text);

      template<class TO>
      static constexpr TO Convert(RTTI::VMeta const& from) noexcept {
         return from.GetCppName();
      }
   };
   
   /// Convert Literal -> Text                                                
   template<CT::Literal T>
   struct ConverterFrom<T, LglsUniqueConverterIndex(T)> {
      static_assert(CT::Decayed<T>, "Strip all decorations first");
      LANGULUS_MORPHISM(Anyness::Text);
   };
}