///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Many.hpp"
#include "../../../source/components/Typed-Static.hpp"


namespace Langulus::Anyness
{

   template<CT::NotVoid> struct TMany;
   template<CT::NotVoid> struct TManyView;

   /// A statically-typed continuous container of variable size that is       
   /// binary-compatible with the type-erased alternative above               
   template<CT::NotVoid T>
   struct TMany : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<>,     // Allocation is referenced      
      Component::DeepOwnership<>,      // Referenced indirections       
      Component::Contiguous,           // Heap memory is continuous     
      Component::IndexedLinear<>,      // Indexed directly              
      Component::Emplacement,          // Allows emplacement            
      Component::Insertion<>,          // Allows insertion              
      Component::InsertionOperators<>, // << and >> insertion           
      Component::Removal,              // Allows removal                
      Component::Assignment,           // Allows assignment             
      Component::TypedStack<DMeta, T>, // Type-constrained              
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::Descriptor,           // Descriptor interface          
      Component::IterationForEach,     // ForEach iteration             
      Component::IterationRange,       // Ranged iteration              
      Component::Comparison,           // Allows for comparison         
      Component::Conversion,           // Allows conversion             
      Component::StateStack<           // Variable state                
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
      struct PickSparseMut : Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, T>
      > {
         using CTTI_Sparse = Yes;
      };
      using  PickSparse = T;
      using  Pick       = Tif<CT::Sparse<T>, PickSparse,    PickDense>;
      using  PickMut    = Tif<CT::Sparse<T>, PickSparseMut, PickDenseMut>;

      // Range selections                                               
      struct PickRangeDenseMut : Container<
         Component::HeapMovable<>,
         Component::Contiguous,
         Component::IndexedLinear<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, T>,
         Component::CountStack<>
      > {};
      using  PickRangeDense = PickRangeDenseMut;
      struct PickRangeSparseMut : Container<
         Component::HeapMovable<>,
         Component::OwnershipStack<0, false>,
         Component::DeepOwnership<>,
         Component::Contiguous,
         Component::IndexedLinear<>,
         Component::Assignment,
         Component::TypedStatic<DMeta, T>,
         Component::CountStack<>,
         Component::ReserveStack<>
      > {};
      using  PickRangeSparse = PickRangeSparseMut;
      using  PickRange       = Tif<CT::Sparse<T>, PickRangeSparse,    PickRangeDense>;
      using  PickRangeMut    = Tif<CT::Sparse<T>, PickRangeSparseMut, PickRangeDenseMut>;

      ///                                                                     
      /// Construction                                                        
      constexpr TMany() noexcept = default;
      constexpr TMany(const TMany&) noexcept;
      constexpr TMany(TMany&&) noexcept;

      template<class A1, class...AN>
      TMany(A1&&, AN&&...) requires CT::RangeInsertable<TMany, A1, AN...>;

      ///                                                                     
      /// Assignment                                                          
      TMany& operator = (TMany const&) noexcept = default;
      TMany& operator = (TMany&&) noexcept = default;

      template<class A1> requires CT::RangeAssignable<TMany, A1>
      TMany& operator = (A1&&);
   };
   
   /// A statically-typed continuous container view of variable size          
   /// Doesn't have ownership, and binary-compatible with the container above 
   template<CT::NotVoid T>
   struct TManyView : Container<
      Component::HeapMovable<>,        // Pointer to heap memory        
      Component::OwnershipStack<0, false>,   // Pointer to an allocation
      Component::Contiguous,           // Heap memory is continuous     
      Component::IndexedLinear<>,      // Indexed directly              
      Component::TypedStack<DMeta, T>, // Type-constrained              
      Component::CountStack<>,         // Variable count                
      Component::ReserveStack<>,       // Variable capacity             
      Component::HashStack<>,          // Variable hash (cached)        
      Component::Descriptor,           // Descriptor interface          
      Component::IterationForEach,     // ForEach iteration             
      Component::IterationRange,       // ForEach iteration             
      Component::Comparison,           // Allows for comparison         
      Component::Conversion,           // Allows conversion             
      Component::StateStack<           // Variable state                
         DefineState::Typed<State::Enabled>, // Always type-constrained 
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Or<>,            // Adds 'or' state               
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using CTTI_ReflectAs = ManyView;
   };

} // namespace Langulus::Anyness
