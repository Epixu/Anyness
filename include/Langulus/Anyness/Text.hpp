///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/Allocator.hpp"
#include "../../../source/components/Heap-Movable.hpp"
#include "../../../source/components/Ownership-Stack.hpp"
#include "../../../source/components/Contiguous.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Heap.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/components/Comparison.hpp"
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

      ///                                                                     
      using TextBase = Container<
         Com::HeapMovable<>,              // Pointer to heap memory     
         Com::OwnershipStack<>,           // Allocation is referenced   
         Com::Contiguous,                 // Heap memory is continuous  
         Com::IndexedLinear<>,            // Indexed directly           
         Com::Emplacement<>,              // Allows emplacement         
         Com::Insertion<0, Text>,         // Serialize + insert         
         Com::InsertionOperators<0, Text>,// << and >> insertion        
         Com::Removal<>,                  // Allows removal             
         Com::Assignment<>,               // Allows assignment          
         Com::TypedStatic<DMeta, char>,   // Type-constrained           
         Com::CountStack<>,               // Variable count             
         Com::ReserveHeap<>,              // Variable capacity          
         Com::HashStack<>,                // Variable hash (cached)     
         Com::Comparison,                 // Comparisons                
         Com::StateStack<                 // Variable state             
            DefineState::Typed<State::Enabled>, // Always typed         
            DefineState::Compressed<>,    // Adds 'compressed' state    
            DefineState::Encrypted<>,     // Adds 'encrypted' state     
            DefineState::Tracked<>        // Adds 'tracked' state       
         >
      >;

   } // namespace Langulus::Anyness::Inner


   ///                                                                        
   /// A continuous text container of variable size                           
   ///                                                                        
   struct Text : Inner::TextBase {
      using Base = Inner::TextBase;
      using CTTI_Text = Yes;

      constexpr Text() noexcept = default;

      template<class A1, class...AN> requires CT::RangeInsertable<Text, A1, AN...>
      Text(A1&&, AN&&...);

      static Text FromText(CT::Text auto&&, CountType);
      static Text FromNumber(CT::Number auto&&, int precision = 0);

      // Single element selections                                      
      using  Pick    = char const&;
      using  PickMut = char&;

      // Range selections                                               
      struct PickRange : Container<
         Com::HeapMovable<>,
         Com::Contiguous,
         Com::IndexedLinear<>,
         Com::TypedStatic<DMeta, char>,
         Com::CountStack<>
      > {};
      struct PickRangeMut : Container<
         Com::HeapMovable<>,
         Com::Contiguous,
         Com::IndexedLinear<>,
         Com::Assignment<>,
         Com::TypedStatic<DMeta, char>,
         Com::CountStack<>
      > {};

      /// Interpret text container as a string_view                           
      ///   @attention the string is null-terminated only after Terminate()   
      constexpr operator Token() const noexcept {
         //TODO moves these to tests
         static_assert(CT::Exact<typename CTTI::Typed<Text>::Type, void>, "Wrongly typed container");
         static_assert(CT::Typed<Text>, "Container not typed");
         static_assert(not CT::Array<Text>, "Wrongly typed container");
         static_assert(CTTI::Void<void>::Enabled, "Wrongly typed container");
         static_assert(not CTTI::Void<void*>::Enabled, "Wrongly typed container");
         static_assert(    CT::Void<typename CTTI::Typed<Text>::Type>, "Wrongly typed container");
         static_assert(not CT::NotVoid<typename CTTI::Typed<Text>::Type>, "Wrongly typed container");
         static_assert(requires { typename Text::CTTI_Typed; }, "Wrongly typed container");
         static_assert(CT::Exact<decltype(CT::Inner::GetUnderlyingType<Text>()), Types<char>>, "Wrongly typed container");
         static_assert(CT::Exact<TypeOf<Text>, char>, "Wrongly typed container");
         return {GetRaw(), GetCount()};
      }

      /// Comparing with other containers or characters                       
      using Base::operator ==;

      /// Comparing against nullptr_t checks if text is empty                 
      constexpr bool operator == (::std::nullptr_t) const noexcept {
         return GetCount() == 0;
      }

      /// Comparing against bounded character arrays and literals             
      //constexpr bool operator == (const CT::TextLiteral auto&) const noexcept;

      /// Comparing against null-terminated strings                           
      constexpr bool operator == (const CT::TextPointer auto& rhs) const noexcept {
         if (rhs == nullptr or *rhs == 0)
            return IsEmpty();
         return operator == (Text {Disown(rhs)});
      }

      /// Comparing against std containers with characters                    
      constexpr bool operator == (const CT::TextRange auto& rhs) const noexcept {
         return operator == (Text {Disown(rhs)});
      }
   };

} // namespace Langulus::Anyness

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

   } // namespace Langulus::CT::Inner

   /// A stringifiable type is one that has either an implicit or explicit    
   /// cast operator to Text type, or can be used to explicitly initialize a  
   /// Text container                                                         
   template<class...T>
   concept Stringifiable = ((Inner::StringifiableByOperator<T>
                          or Inner::StringifiableByConstructor<T>) and ...);

} // namespace Langulus::CT

namespace Langulus
{

   /// Make a text literal                                                    
   Anyness::Text operator ""_text(const char* text, ::std::size_t size) {
      static_assert(CTTI::Sparse<const char*>::Enabled);
      static_assert(CT::Sparse<const char*>);
      static_assert(CT::Character<char>);
      static_assert(CT::Text<Disown<const char*>>);
      return Anyness::Text::FromText(Disown(text), size);
   }

} // namespace Langulus
