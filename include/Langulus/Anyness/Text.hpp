///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Anyness/THandle.hpp>
#include <Langulus/Anyness/TextView.hpp>
#include "../../../source/Container.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Concatenate.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Emergent.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/states/Typed.hpp"
#include <Langulus/CT/Text.hpp>
#include <Langulus/CT/Number.hpp>


namespace Langulus::Anyness
{
   struct Text;

   namespace Inner
   {
      using TextBase = Container<
         Com::HeapMovable<>,              // Pointer to heap memory     
         Com::OwnershipStack<>,           // Allocation is referenced   
         Com::IndexedLinear<>,            // Indexed directly           
         Com::Emplacement<>,              // Allows emplacement         
         Com::Insertion<0, Text>,         // Serialize + insert         
         Com::InsertionOperators<0, Text>,// << and >> insertion        
         Com::Concatenate<>,              // Concatenate                
         Com::Removal<>,                  // Allows removal             
         Com::Assignment<>,               // Allows assignment          
         Com::TypedStack<DMeta, char>,    // Type-constrained           
         Com::CountStack<>,               // Variable count             
         Com::ReserveEmergent<>,          // Variable capacity          
         Com::HashStack<>,                // Variable hash (cached)     
         Com::IterationForEach<>,         // ForEach iteration          
         Com::IterationRange<>,           // Range iteration            
         Com::Comparison,                 // Allows for comparison      
         Com::Conversion,                 // Allows conversion          
         Com::StateStack<                 // Variable state             
            DefineState::Typed<State::Enabled>, // Always typed         
            DefineState::Compressed<>,    // Adds 'compressed' state    
            DefineState::Encrypted<>,     // Adds 'encrypted' state     
            DefineState::Tracked<>        // Adds 'tracked' state       
         >
      >;
   }


   ///                                                                        
   /// A continuous text container of variable size                           
   ///                                                                        
   struct Text : Inner::TextBase {
      using Base = Inner::TextBase;
      using CountType = Base::CountType;
      using CTTI_Text = Yes<>;
      using Base::Base;

      // Single element selections                                      
      using Pick = char const&;
      using PickMut = char&;

      // Range selections                                               
      using PickRange = TextView;

      struct PickRangeMut : PickRange::AddComponents<Com::Assignment<>> {};

   public:
      constexpr Text(nullptr_t) noexcept {}

      /// Construction from any kind of text that isn't an Anyness container  
      template<CT::Text T> requires CT::NotContainer<T>
      constexpr Text(T&& text) {
         using S  = IntentOf<T&&>;
         using ST = TypeOf<S>;
         decltype(auto) source = DeintCast(FWD(text));

         if constexpr (CT::TextLiteral<ST>) {
            // Create from a text literal/bounded array                 
            // Type can be either char, or const char                   
            using CHAR = TypeOf<ST>;
            static_assert(CT::Similar<CHAR, char>, "Type mismatch");
            this->mType = MetaDataOf<CHAR>();
            this->mHeapReadable = DecvqAllCast(source);
            SetCount(strnlen(source, ExtentOf<T>));
         }
         else if constexpr (CT::TextPointer<ST>) {
            // Create from a null-terminated char pointer               
            // Type can be either char, or const char                   
            if (not source)
               return;
            using CHAR = Deptr<ST>;
            static_assert(CT::Similar<CHAR, char>, "Type mismatch");
            this->mType = MetaDataOf<CHAR>();
            this->mHeapReadable = DecvqAllCast(source);
            SetCount(strlen(source));
         }
         else if constexpr (::std::ranges::contiguous_range<ST>) {
            // Create from an std container                             
            // Type can be either char, or const char                   
            if (source.empty())
               return;
            using CHAR = Deptr<decltype(source.data())>;
            static_assert(CT::Similar<CHAR, char>, "Type mismatch");
            this->mType = MetaDataOf<CHAR>();
            this->mHeapReadable = source.data();
            SetCount(source.size());
         }
         else static_assert(false, "Unsupported text constructor");

         // Make sure we start off without ownership                    
         if constexpr (requires { SetAllocation(nullptr); })
            SetAllocation(nullptr);

         // Take ownership if the intent requires it                    
         if constexpr (S::KeepsOnCopy() and requires { TakeOwnership(); })
            TakeOwnership();
      }

      /// Construction from all kinds of characters                           
      template<CT::Character T>
      constexpr Text(T&& ch) {
         this->mType = MetaDataOf<char>();
         AllocateFresh(RequestSize(1));
         *GetRaw() = DeintCast(ch);
         SetCount(1);
      }

      //template<class A1, class...AN>
      //Text(A1&&, AN&&...) requires CT::RangeInsertable<Text, A1, AN...>;

      /// Construction from all kinds of text, trim length to desired count   
      ///   @param text - text to wrap, assumed valid                         
      ///   @param count - number of characters inside 'text' to use          
      template<CT::Text T>
      static Text FromText(T&& text, CountType count) {
         if (count == 0)
            return {};

         Text result {FWD(text)};
         if (count < result.GetCount())
            result.SetCount(count);
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
               result.AllocateFresh(result.RequestSize(c));
               memcpy(result.mHeap, temp, c);
               result.SetCount(c);
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
                  else break;
               }
               else break;
            }

            ++lastChar;
            const auto c = static_cast<CountType>(lastChar - temp);
            if (approximate) {
               // We've truncated the number, so prepend a '~' symbol to
               // signify it's an approximate representation            
               result.AllocateFresh(result.RequestSize(c + 1));
               *result.mHeap = '~';
               memcpy(result.mHeap + 1, temp, c);
               result.SetCount(c + 1);
            }
            else {
               result.AllocateFresh(result.RequestSize(c));
               memcpy(result.mHeap, temp, c);
               result.SetCount(c);
            }
         }
         else if constexpr (CT::Integer<T>) {
            // Stringify an integer                                     
            constexpr auto size = ::std::numeric_limits<T>::digits10 * 2;
            char temp[size];
            auto [lastChar, errorCode] = ::std::to_chars(temp, temp + size, number);
            LglsAssert(errorCode == ::std::errc(), "std::to_chars failure");

            const auto c = static_cast<CountType>(lastChar - temp);
            result.AllocateFresh(result.RequestSize(c));
            memcpy(result.mHeap, temp, c);
            result.SetCount(c);
         }
         else static_assert(false, "Unsupported number type");
         return result;
      }

      /// Interpret text container as a string_view                           
      ///   @attention the string is null-terminated only after Terminate()   
      constexpr operator Token() const noexcept {
         return {this->GetRaw(), this->GetCount()};
      }

      /// Comparing with other containers or characters                       
      using Base::operator ==;

      /// Comparing against nullptr_t checks if text is empty                 
      constexpr bool operator == (nullptr_t) const noexcept {
         return this->GetCount() == 0;
      }

      /// Comparing against bounded character arrays and literals             
      //constexpr bool operator == (const CT::TextLiteral auto&) const noexcept;

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

      /// Custom concatenation operator that includes string literals,        
      /// null-terminated string pointers, and std::continuous_ranges         
      ///   @note conventional container concatenation is defined in          
      ///      Com::Concatenate and Com::ConcatenateOperators                 
      template<CT::Text T> requires CT::NotContainer<T>
      Text& operator += (T&& rhs) {
         if (IsEmpty()) {
            *this = Text {FWD(rhs)};
            return *this;
         }

         using DT = Deint<T>;
         decltype(auto) source = DeintCast(FWD(rhs));
         const auto currentCount = GetCount();

         if constexpr (CT::TextLiteral<DT>) {
            // Create from a text literal/bounded array                 
            using CHAR = TypeOf<DT>;
            static_assert(::std::same_as<Decvq<CHAR>, char>, "Type mismatch");
            const auto count = strnlen(source, ExtentOf<DT>);
            AllocateMore(currentCount + count);
            memcpy(mHeap + currentCount, source, count);
            SetCount(currentCount + count);
         }
         else if constexpr (CT::TextPointer<DT>) {
            // Create from a null-terminated char pointer               
            if (not source)
               return *this;
            using CHAR = Deptr<DT>;
            static_assert(::std::same_as<Decvq<CHAR>, char>, "Type mismatch");
            const auto count = strlen(source);
            AllocateMore(currentCount + count);
            memcpy(mHeap + currentCount, source, count);
            SetCount(currentCount + count);
         }
         else if constexpr (::std::ranges::contiguous_range<DT>) {
            // Create from an std container                             
            if (source.empty())
               return *this;
            using CHAR = Deptr<decltype(source.data())>;
            static_assert(::std::same_as<Decvq<CHAR>, char>, "Type mismatch");
            const auto count = source.size();
            AllocateMore(currentCount + count);
            memcpy(mHeap + currentCount, source.data(), count);
            SetCount(currentCount + count);
         }
         else static_assert(false, "Unsupported text concatenation");
         return *this;
      }
   };
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
   Anyness::Text operator ""_text(const char* text, size_t size) {
      return Anyness::Text::FromText(Disown(text), size);
   }
}
