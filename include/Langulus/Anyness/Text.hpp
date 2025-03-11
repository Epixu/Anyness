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
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Tracked.hpp"
#include <Langulus/CT/Text.hpp>


namespace Langulus::Anyness
{

   using DMeta = RTTI::DMeta;

   ///                                                                        
   /// A continuous text container of variable size                           
   ///                                                                        
   struct Text : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::Contiguous,           // Heap memory is continuous     
      Component::IndexedLinear<>,      // Indexed directly              
      Component::Emplacement,          // Allows emplacement            
      Component::Insertion<Text>,            // Serialize + insert      
      Component::InsertionOperators<Text>,   // << and >> insertion     
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStatic<DMeta, char>,   // Type-constrained        
      Component::CountStack<>,         // Variable count                
      Component::ReserveHeap<>,        // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::StateStack<           // Variable state                
         State::Compressed<>,          // Adds 'compressed' state       
         State::Encrypted<>,           // Adds 'encrypted' state        
         State::Tracked<>              // Adds 'tracked' state          
      >
   > {
      using CTTI_Text = Yes;

      template<class A1, class...AN> requires RangeInsertable<Text, A1, AN...>
      Text(A1&&, AN&&...);

      // Single element selections                                      
      using  Pick    = char const&;
      using  PickMut = char&;

      // Range selections                                               
      struct PickRange : Container<
         Component::HeapMovable<>,
         Component::Contiguous,
         Component::IndexedLinear<>,
         Component::TypedStatic<DMeta, char>,
         Component::CountStack<>
      > {};
      struct PickRangeMut : Container<
         Component::HeapMovable<>,
         Component::Contiguous,
         Component::IndexedLinear<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, char>,
         Component::CountStack<>
      > {};

      /// Interpret text container as a string_view                           
      ///   @attention the string is null-terminated only after Terminate()   
      operator Token() const noexcept {
         return {GetRaw(), GetCount()};
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