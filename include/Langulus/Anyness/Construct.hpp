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
#include "../../../source/components/Charge-Stack.hpp"
#include "../../../source/states/Typed.hpp"
#include "../../../source/states/Future.hpp"
#include "../../../source/states/Past.hpp"
#include "../../../source/states/Compressed.hpp"
#include "../../../source/states/Encrypted.hpp"
#include "../../../source/states/Or.hpp"
#include "../../../source/states/Tracked.hpp"
#include "../../../source/rtti/MetaData.hpp"
#include "Handle.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   ///   Construct                                                            
   ///                                                                        
   ///   Used to contain constructor arguments for any type. It is just a     
   /// type-erased Many, but also carries a charge and a type. It is often    
   /// used in Verbs::Create to provide instructions on how to instantiate a  
   /// data type.                                                             
   ///                                                                        
   struct Construct : Container<
      // Some additional data                                           
      Com::TypedStack<DMeta, void, 1>, // What are we constructing?     
      Com::Charge,                     // How many, when?               
      // The rest is just a Many for the descriptor                     
      Com::HeapMovable<>,              // Pointer to heap memory        
      Com::OwnershipStack<>,           // Allocation is referenced      
      Com::DeepOwnershipHeap<>,        // Referenced indirections       
      Com::Contiguous,                 // Heap memory is continuous     
      Com::IndexedLinear<>,            // Indexed directly              
      Com::Insertion<>,                // Allows insertion              
      Com::InsertionOperators<>,       // << and >> insertion           
      Com::Emplacement<>,              // Allows emplacement            
      Com::Removal<>,                  // Allows removal                
      Com::Assignment<>,               // Allows assignment             
      Com::TypedStack<DMeta>,          // Variable type                 
      Com::CountStack<>,               // Variable count                
      Com::ReserveStack<>,             // Variable capacity             
      Com::HashStack<>,                // Variable hash (cached)        
      Com::Descriptor,                 // Descriptor interface          
      Com::IterationForEach<>,         // ForEach iteration             
      Com::IterationRange<>,           // Ranged iteration              
      Com::Comparison,                 // Allows for comparison         
      Com::Conversion,                 // Allows conversion             
      Com::StateStack<                 // Variable state                
         DefineState::Typed<>,         // Can be type-constrained       
         DefineState::Future<>,        // Adds a 'missing future' state 
         DefineState::Past<>,          // Adds a 'missing past' state   
         DefineState::Compressed<>,    // Adds 'compressed' state       
         DefineState::Encrypted<>,     // Adds 'encrypted' state        
         DefineState::Or<>,            // Adds 'or' state               
         DefineState::Tracked<>        // Adds 'tracked' state          
      >
   > {
      using Charge = Com::Charge;

      constexpr Construct() noexcept = default;
      Construct(const Construct&) noexcept;
      Construct(Construct&&) noexcept;

      Construct(DMeta);
      Construct(DMeta, auto&&, const Charge& = {});

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         Construct(const Token&);
         Construct(const Token&, auto&&, const Charge& = {});
      #endif

      template<CT::NotVoid, CT::NotVoid A1, CT::NotVoid...AN>
      static Construct From(A1&&, AN&&...);
      template<CT::NotVoid>
      static Construct From();

      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         template<CT::NotVoid A1, CT::NotVoid...AN>
         static Construct FromToken(const Token&, A1&&, AN&&...);
         static Construct FromToken(const Token&);
      #endif

      Hash GetHash() const;
      auto GetProducer() const noexcept -> DMeta;
      void Clear();
      void Reset();
      void ResetCharge() noexcept;
   };

} // namespace Langulus::Anyness
