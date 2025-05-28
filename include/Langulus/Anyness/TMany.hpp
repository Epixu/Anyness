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
#include "../../../source/components/DeepOwnership-Heap.hpp"
#include "../../../source/components/Contiguous.hpp"
#include "../../../source/components/Indexed-Linear.hpp"
#include "../../../source/components/Insertion.hpp"
#include "../../../source/components/InsertionOperators.hpp"
#include "../../../source/components/Emplacement.hpp"
#include "../../../source/components/Removal.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Stack.hpp"
#include "../../../source/components/Count-Stack.hpp"
#include "../../../source/components/Reserve-Stack.hpp"
#include "../../../source/components/Hash-Stack.hpp"
#include "../../../source/components/Descriptor.hpp"
#include "../../../source/components/Iteration-ForEach.hpp"
#include "../../../source/components/Iteration-Range.hpp"
#include "../../../source/components/Comparison.hpp"
#include "../../../source/components/Conversion.hpp"
#include "../../../source/components/State-Stack.hpp"
#include "../../../source/states/Typed.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Or.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/rtti/MetaData.hpp"
#include "THandle.hpp"


namespace Langulus::Anyness
{

   struct Many;
   template<CT::NotVoid> struct TMany;
   template<CT::NotVoid> struct TManyView;


   ///                                                                        
   /// A statically-typed contiguous container of variable size that is       
   /// binary-compatible with the type-erased alternative above               
   ///                                                                        
   template<CT::NotVoid T>
   struct TMany : Container<
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::DeepOwnershipHeap<>,        // Referenced indirections       
      Com::Contiguous,                 // Heap memory is continuous     
      Com::IndexedLinear<>,            // Indexed directly              
      Com::Emplacement<>,              // Allows emplacement            
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta, T>,       // Type-constrained              
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::HashStack<>,                // Variable hash (cached)        
      Com::Descriptor,                 // Descriptor interface          
      Com::IterationForEach<>,         // ForEach iteration             
      Com::IterationRange<>,           // Ranged iteration              
      Com::Comparison,                 // Allows for comparison         
      Com::Conversion,                 // Allows conversion             
      Com::StateStack<                 // Variable state                
         DefineState::Typed<State::Enabled>, // Always type-constrained 
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Or<>,            // Adds 'or' state               
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = Many;

      // View                                                           
      using  ViewType = TManyView<T>;

      // Deep type                                                      
      using  DeepType = Many;

      // Single element selections                                      
      using  PickDenseMut  = T&;
      using  PickDense     = T const&;
      using  PickSparseMut = THandle<T&>;
      using  PickSparse    = THandle<T const&>;
      using  Pick          = Tif<CT::Sparse<T>, PickSparse,    PickDense>;
      using  PickMut       = Tif<CT::Sparse<T>, PickSparseMut, PickDenseMut>;

      // Range selections                                               
      struct PickRangeDenseMut : Container<
         Com::HeapMovable<>,
         Com::Contiguous,
         Com::IndexedLinear<>,
         Com::Assignment<>,
         Com::TypedStatic<DMeta, T>,
         Com::CountStack<>
      > {};
      using  PickRangeDense = PickRangeDenseMut;
      struct PickRangeSparseMut : Container<
         Com::HeapMovable<>,
         Com::OwnershipStack<0, false>,
         Com::DeepOwnershipHeap<>,
         Com::Contiguous,
         Com::IndexedLinear<>,
         Com::Assignment<>,
         Com::TypedStatic<DMeta, T>,
         Com::CountStack<>,
         Com::ReserveStack<>
      > {};
      using  PickRangeSparse = PickRangeSparseMut;
      using  PickRange       = Tif<CT::Sparse<T>, PickRangeSparse,    PickRangeDense>;
      using  PickRangeMut    = Tif<CT::Sparse<T>, PickRangeSparseMut, PickRangeDenseMut>;

      ///                                                                     
      /// Construction                                                        
      constexpr TMany() noexcept = default;
      constexpr TMany(const TMany&) noexcept = default;
      constexpr TMany(TMany&&) noexcept = default;

      template<class A1, class...AN>
      TMany(A1&&, AN&&...) requires CT::RangeInsertable<TMany, A1, AN...>;

      ///                                                                     
      /// Assignment                                                          
      TMany& operator = (TMany const&) noexcept = default;
      TMany& operator = (TMany&&) noexcept = default;

      template<class A1> requires CT::RangeAssignable<TMany, A1>
      TMany& operator = (A1&&);
   };
   
} // namespace Langulus::Anyness
