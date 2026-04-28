///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Utils/Tuple.hpp>
#include <Langulus/CT/Contiguous.hpp>


namespace Langulus::CTTI
{
   /// Affects CT::State<T>                                                   
   template<class T> struct State;
   
   /// Affects CT::Component<T>                                               
   template<class T> struct Component;
   
   /// Affects CT::Container<T>                                               
   template<class T> struct Container;
   
   /// Affects CT::Map<T>                                                     
   template<class T> struct Map;
   
   /// Affects CT::Set<T>                                                     
   template<class T> struct Set;
   
   /// Affects CT::Pair<T>                                                    
   template<class T> struct Pair;

   /// Affects CT::Handle<T>                                                  
   template<class T> struct Handle;

   /// Affects CT::Iterator<T>                                                
   template<class T> struct Iterator;
}

LANGULUS_CTTI_CONCEPT_DECVQ(State);
LANGULUS_CTTI_CONCEPT_DECVQ(Component);
LANGULUS_CTTI_CONCEPT_DECVQ(Container);
LANGULUS_CTTI_CONCEPT_DECVQ(Map);
LANGULUS_CTTI_CONCEPT_DECVQ(Set);
LANGULUS_CTTI_CONCEPT_DECVQ(Pair);
LANGULUS_CTTI_CONCEPT_DECVQ(Handle);
LANGULUS_CTTI_CONCEPT_DECVQ(Iterator);

namespace Langulus::Anyness
{
   /// A component ID                                                         
   using Cid = uint;
}

namespace Langulus::Anyness::Component
{
   enum OwnershipStyle {
      NoOwnership = 0,
      OnCreate = 1,
      OnAssign = 2,
      OnDestroy = 4
   };

   constexpr uint WeakOwnership = OnAssign;
   constexpr uint StrongOwnership = OnCreate | OnAssign | OnDestroy;

   /// Get the dimensions of a container                                      
   /// This is usually defined in the count component, and depicts the        
   /// 'horizontal' size of a container. A pair, for example, has two         
   /// dimensions, despite having a count of 1. A fair analogy would be a     
   /// matrix: where a 4x4 matrix will have count of 4 in 4 dimensions.       
   template<class T>
   using Dimensions = typename ShedDeref<T>::Dimensions;

   template<Cid SID, Cid...IDS>
   concept IdMatch = ((SID == IDS) or ...);
}

namespace Langulus::CT
{
   /// Check if listed types are containers with any kind of Ownership        
   /// component                                                              
   template<class...T>
   concept Owned = Container<T...>
       and ((ShedDeref<T>::Owned != 0) and ...);

   /// Check if two containers/elements have the same dimensions              
   template<class LHS, class RHS>
   concept CompatibleDimensions =
      (Container<LHS, RHS> and Same<Anyness::Component::Dimensions<LHS>, Anyness::Component::Dimensions<RHS>>)
        or (Container<LHS> and not Container<RHS> and Same<Anyness::Component::Dimensions<LHS>, Values<Anyness::Cid(0)>>)
        or (Container<RHS> and not Container<LHS> and Same<Anyness::Component::Dimensions<RHS>, Values<Anyness::Cid(0)>>);

   /// Check if listed containers are referenced upon construction/assignment 
   /// and then automatically dereferenced on destruction                     
   template<class...T>
   concept StronglyOwned = Container<T...>
       and (((ShedDeref<T>::Owned & Anyness::Component::StrongOwnership) != 0) and ...);
   
   /// Check if listed types are containers with any kind of heap memory      
   template<class...T>
   concept HeapAllocated = Container<T...>
       and ((ShedDeref<T>::CountHeapProviders() > 0) and ...);
   
   /// Check if listed types are containers with more than one heap provider, 
   /// such as maps.                                                          
   template<class...T>
   concept Multiheap = Container<T...>
       and ((ShedDeref<T>::CountHeapProviders() > 1) and ...);
   
   /// Check if listed types are containers with variable count               
   ///   @attention this includes containers with Com::CountStatic, but have  
   ///      nullifiable heap pointer                                          
   template<class...T>
   concept HasVariableCount = HeapAllocated<T...>
       and (ShedDeref<T>::HeapCanBeNull and ...);
   
   /// Check if listed types are containers that can have multiple elements   
   template<class...T>
   concept ContainsMany = Container<T...>
       and (ShedDeref<T>::ContainsMany and ...);
   
   /// Check if listed types are containers that can have single element      
   template<class...T>
   concept ContainsOne = Container<T...>
       and ((not ShedDeref<T>::ContainsMany) and ...);
   
   /// Check if listed types are type-erased containers                       
   template<class...T>
   concept TypeErased = Container<T...>
       and ((ShedDeref<T>::TypeErased) and ...);

   /// Check if listed types are containers with any kind of DeepOwnership    
   /// component                                                              
   template<class...T>
   concept DeeplyOwned = Container<T...>
       and (ShedDeref<T>::DeeplyOwned and ...)
       and ((CT::TypeErased<T> or CT::Sparse<TypeOf<T>>) and ...);

   /// Check if listed types are containers, and are indexed                  
   template<class...T>
   concept Indexed = Container<T...>
       and ((ShedDeref<T>::Indexed) and ...);

   /// Check if listed types are containers, and are linearly indexed         
   template<class...T>
   concept IndexedLinearly = Indexed<T...> and Contiguous<T...>;

   /// Check if listed types are containers, and are linearly indexed         
   template<class T>
   concept HeapEntry = requires {
      {T::Id} -> Same<uint>;
      CT::Sparse<typename T::T>;
   };
}

namespace Langulus::Anyness
{
   /// Used for requesting dynamic data from the heap in container components.
   /// @important Requests with this modifier are positioned after elements   
   ///   in order to avoid moving elements as header gets resized.            
   template<class T>
   struct PerElement {
      static constexpr bool AllocatedPerElement = true;
      using Type = T;
   };

   /// Used for requesting dynamic data from the heap in container components 
   /// @important Requests with this modifier are positioned after elements   
   ///   in order to avoid moving elements as header gets resized.            
   template<class T>
   struct PerIndirection {
      static constexpr bool AllocatedPerIndirection = true;
      using Type = T;
   };

   /// A helper structure for pairing heap components with type components    
   ///   @tparam ID - the type component ID                                   
   ///   @tparam POINTER_TYPE - associated heap pointer type - mainly a       
   ///      customization point for packed pointer use.                       
   template<Cid ID = 0, CT::Sparse POINTER_TYPE = void*>
   struct HeapEntry {
      static constexpr Cid Id = ID;
      using T = POINTER_TYPE;
   };

   enum class StateValue {
      Variable = 0,
      Enabled  = 1,
      Disabled = 2
   };

   enum class StateUid {
      Invalid,
      Compressed,
      Encrypted,
      Future,
      Past,
      Or,
      Sorted,
      Tracked,
      Typed
   };

   namespace Component
   {
      ///                                                                     
      ///   COMPONENT CATALOGUE                                               
      ///                                                                     
      
      /// Type providers                                                      
      template<class META, class TYPE = void, bool CONSTRAIN = not ::std::is_void_v<TYPE>, Cid = 0> struct TypedStack;
      #define LglsComTypedStack(modifier) \
         template<class, class, bool, Cid> modifier struct TypedStack

      template<class META, CT::NotVoid TYPE, Cid = 0> struct TypedStatic;
      #define LglsComTypedStatic(modifier) \
         template<class, CT::NotVoid, Cid> modifier struct TypedStatic

      /// Data providers                                                      
      template<Cid = 0, CT::HeapEntry...> struct HeapReference;
      #define LglsComHeapReference(modifier) \
         template<Cid, CT::HeapEntry...> modifier struct HeapReference

      template<Cid = 0, uint = 0, uint = 0, CT::HeapEntry...> struct HeapImmovable;
      #define LglsComHeapImmovable(modifier) \
         template<Cid, uint, uint, CT::HeapEntry...> modifier struct HeapImmovable

      template<Cid = 0, uint = 0, uint = 0, CT::HeapEntry...> struct HeapMovable;
      #define LglsComHeapMovable(modifier) \
         template<Cid, uint, uint, CT::HeapEntry...> modifier struct HeapMovable

      template<CT::NotVoid, Cid = 0> struct Stack;
      #define LglsComStack(modifier) \
         template<CT::NotVoid, Cid> modifier struct Stack

      /// Count, reserve, dimensions                                          
      template<Cid = 0, class T = size_t, Cid...> struct CountHeap;
      #define LglsComCountHeap(modifier) \
         template<Cid, class, Cid...> modifier struct CountHeap

      template<Cid = 0, class T = size_t, Cid...> struct CountStack;
      #define LglsComCountStack(modifier) \
         template<Cid, class, Cid...> modifier struct CountStack

      template<Cid = 0, auto COUNT = 0u, Cid...> struct CountStatic;
      #define LglsComCountStatic(modifier) \
         template<Cid, auto, Cid...> modifier struct CountStatic

      template<Cid = 0, class T = size_t, Cid...> struct ReserveEmergent;
      #define LglsComReserveEmergent(modifier) \
         template<Cid, class, Cid...> modifier struct ReserveEmergent

      template<Cid = 0, class T = size_t, Cid...> struct ReserveStack;
      #define LglsComReserveStack(modifier) \
         template<Cid, class, Cid...> modifier struct ReserveStack

      template<Cid = 0, auto COUNT = 0u, Cid...> struct ReserveStatic;
      #define LglsComReserveStatic(modifier) \
         template<Cid, auto, Cid...> modifier struct ReserveStatic
      
      /// Ownership                                                           
      template<Cid = 0, uint = StrongOwnership, Cid...> struct OwnershipEmergent;
      #define LglsComOwnershipEmergent(modifier) \
         template<Cid, uint, Cid...> modifier struct OwnershipEmergent

      template<Cid = 0, uint = StrongOwnership, Cid...> struct OwnershipStack;
      #define LglsComOwnershipStack(modifier) \
         template<Cid, uint, Cid...> modifier struct OwnershipStack

      template<Cid = 0, bool REF_INDIVIDUAL = true, Cid...> struct OwnershipDeepReference;
      #define LglsComOwnershipDeepReference(modifier) \
         template<Cid, bool, Cid...> modifier struct OwnershipDeepReference

      template<Cid = 0, bool REF_INDIVIDUAL = true, Cid...> struct OwnershipDeepEmergent;
      #define LglsComOwnershipDeepEmergent(modifier) \
         template<Cid, bool, Cid...> modifier struct OwnershipDeepEmergent

      template<Cid = 0, bool REF_INDIVIDUAL = true, Cid...> struct OwnershipDeepHeap;
      #define LglsComOwnershipDeepHeap(modifier) \
         template<Cid, bool, Cid...> modifier struct OwnershipDeepHeap

      /// Hashing                                                             
      template<Cid = 0, class H = Hash, Cid...> struct HashEmergent;
      #define LglsComHashEmergent(modifier) \
         template<Cid, class, Cid...> modifier struct HashEmergent

      template<Cid = 0, class H = Hash, Cid...> struct HashHeap;
      #define LglsComHashHeap(modifier) \
         template<Cid, class, Cid...> modifier struct HashHeap

      template<Cid = 0, class H = Hash, Cid...> struct HashStack;
      #define LglsComHashStack(modifier) \
         template<Cid, class, Cid...> modifier struct HashStack

      /// Indexing                                                            
      template<Cid = 0, Cid...> struct IndexedCommon;
      #define LglsComIndexedCommon(modifier) \
         template<Cid, Cid...> modifier struct IndexedCommon

      template<Cid = 0, Cid...> struct IndexedLinear;
      #define LglsComIndexedLinear(modifier) \
         template<Cid, Cid...> modifier struct IndexedLinear

      template<Cid = 0, class H = Hash, Cid...> struct IndexedCommonHashed;
      #define LglsComIndexedCommonHashed(modifier) \
         template<Cid, class, Cid...> modifier struct IndexedCommonHashed

      template<Cid = 0, class H = Hash, Cid...> struct IndexedHashHeap;
      #define LglsComIndexedHashHeap(modifier) \
         template<Cid, class, Cid...> modifier struct IndexedHashHeap

      template<Cid = 0, class H = Hash, Cid...> struct IndexedHashStack;
      #define LglsComIndexedHashStack(modifier) \
         template<Cid, class, Cid...> modifier struct IndexedHashStack

      /// Iteration                                                           
      template<Cid = 0, Cid...> struct IterationForEach;
      #define LglsComIterationForEach(modifier) \
         template<Cid, Cid...> modifier struct IterationForEach

      template<Cid = 0, Cid...> struct IterationRange;
      #define LglsComIterationRange(modifier) \
         template<Cid, Cid...> modifier struct IterationRange

      template<Cid = 0, Cid...> struct IterationOperators;
      #define LglsComIterationOperators(modifier) \
         template<Cid, Cid...> modifier struct IterationOperators

      /// States                                                              
      /*template<CT::State...> struct StateHeap;
      #define LglsComStateHeap(modifier) \
         template<CT::State...> modifier struct StateHeap*/

      template<CT::State...> struct StateStack;
      #define LglsComStateStack(modifier) \
         template<CT::State...> modifier struct StateStack

      /*template<CT::State...> struct StateStatic;
      #define LglsComStateStatic(modifier) \
         template<CT::State...> modifier struct StateStatic*/

      namespace State
      {
         template<StateValue = StateValue::Variable, Cid = 0, Cid...> struct Compressed;
         #define LglsStateCompressed(modifier) \
            template<StateValue, Cid, Cid...> modifier struct State::Compressed

         template<StateValue = StateValue::Variable, Cid = 0, Cid...> struct Encrypted;
         #define LglsStateEncrypted(modifier) \
            template<StateValue, Cid, Cid...> modifier struct State::Encrypted

         template<StateValue = StateValue::Variable, Cid = 0, Cid...> struct Future;
         #define LglsStateFuture(modifier) \
            template<StateValue, Cid, Cid...> modifier struct State::Future

         template<StateValue = StateValue::Variable, Cid = 0, Cid...> struct Or;
         #define LglsStateOr(modifier) \
            template<StateValue, Cid, Cid...> modifier struct State::Or

         template<StateValue = StateValue::Variable, Cid = 0, Cid...> struct Past;
         #define LglsStatePast(modifier) \
            template<StateValue, Cid, Cid...> modifier struct State::Past

         template<StateValue = StateValue::Variable, Cid = 0, Cid...> struct Sorted;
         #define LglsStateSorted(modifier) \
            template<StateValue, Cid, Cid...> modifier struct State::Sorted

         template<StateValue = StateValue::Variable, Cid = 0, Cid...> struct Tracked;
         #define LglsStateTracked(modifier) \
            template<StateValue, Cid, Cid...> modifier struct State::Tracked

         template<StateValue = StateValue::Variable, Cid = 0, Cid...> struct Typed;
         #define LglsStateTyped(modifier) \
            template<StateValue, Cid, Cid...> modifier struct State::Typed
      }

      namespace StateInner
      {
         /// Go through all components and accumulate their state requests    
         /// into a StateStack component                                      
         template<CT::Component C1, CT::Component...CN>
         consteval auto DefineStates() {
            if constexpr (requires { typename C1::StateRequest; }) {
               if constexpr (CT::NotVoid<typename C1::StateRequest>) {
                  static_assert(CT::State<typename C1::StateRequest>);
                  Types<typename C1::StateRequest> first;

                  if constexpr (sizeof...(CN))
                     return first + DefineStates<CN...>();
                  else
                     return first;
               }
               else {
                  if constexpr (sizeof...(CN))
                     return DefineStates<CN...>();
                  else
                     return NoTypes {};
               }
            }
            else {
               if constexpr (sizeof...(CN))
                  return DefineStates<CN...>();
               else
                  return NoTypes {};
            }
         }

         template<CT::State...STATES>
         consteval auto DecideStateComponent(Types<STATES...>) -> StateStack<STATES...>;
      }

      template<CT::Component...COMPONENTS>
      using DecideStateComponent = decltype(
         StateInner::DecideStateComponent(
            decltype(StateInner::DefineStates<COMPONENTS...>()) {}
         )
      );

      /// Comparison, search, pattern matching                                
      template<Cid = 0, bool HASH = true, Cid...> struct Comparison;
      #define LglsComComparison(modifier) \
         template<Cid, bool, Cid...> modifier struct Comparison

      /// Conversion                                                          
      template<Cid = 0, Cid...> struct Conversion;
      #define LglsComConversion(modifier) \
         template<Cid, Cid...> modifier struct Conversion

      /// Emplacement, insertion, merging, concatenation, removal             
      template<Cid = 0, Cid...> struct Emplacement;
      #define LglsComEmplacement(modifier) \
         template<Cid, Cid...> modifier struct Emplacement

      template<Cid = 0, Cid...> struct Assignment;
      #define LglsComAssignment(modifier) \
         template<Cid, Cid...> modifier struct Assignment

      template<Cid = 0, class AS = void, Cid...> struct Insertion;
      #define LglsComInsertion(modifier) \
         template<Cid, class, Cid...> modifier struct Insertion

      template<Cid = 0, class AS = void, Cid...> struct InsertionOperators;
      #define LglsComInsertionOperators(modifier) \
         template<Cid, class, Cid...> modifier struct InsertionOperators

      template<Cid = 0, class AS = void, Cid...> struct InsertionOperatorsConcat;
      #define LglsComInsertionOperatorsConcat(modifier) \
         template<Cid, class, Cid...> modifier struct InsertionOperatorsConcat

      template<Cid = 0, class AS = void, Cid...> struct Merging;
      #define LglsComMerging(modifier) \
         template<Cid, class, Cid...> modifier struct Merging

      template<Cid = 0, class AS = void, Cid...> struct MergingOperators;
      #define LglsComMergingOperators(modifier) \
         template<Cid, class, Cid...> modifier struct MergingOperators

      template<Cid = 0, Cid...> struct Removal;
      #define LglsComRemoval(modifier) \
         template<Cid, Cid...> modifier struct Removal

      /// Other services                                                      
      struct Descriptor;
      struct Charge;

      template<Cid = 0> struct Extrapolation;
      #define LglsComExtrapolation(modifier) \
            template<Cid> modifier struct Extrapolation

      template<Cid = 0> struct Interpolation;
      #define LglsComInterpolation(modifier) \
         template<Cid> modifier struct Interpolation
   }
   
   namespace Com = Component;

   namespace Inner
   {
      
      /// Validate all used components in a container are properly ordered,   
      /// of standard layout, and containing proper sequential provider IDs.  
      ///   @tparam ACC accumulated stack/heap provider IDs                   
      ///   @tparam C1, CN... components                                      
      template<int ACC, int PRECEDENCE, class C1, class...CN>
      consteval bool ValidateComponentOrderNested() {
         static_assert(C1::ComponentPrecedence >= PRECEDENCE,
            "Wrong component order");
         
         if constexpr (requires { C1::StackProvider; }) {
            static_assert(C1::StackProvider == ACC,
               "Invalid stack provider ID");
            static_assert(not requires { C1::HeapProvider; }, 
               "Component can't be both a stack and a heap provider");

            if constexpr (sizeof...(CN) > 0)
               return ValidateComponentOrderNested<ACC + 1, C1::ComponentPrecedence, CN...>();
            else
               return true;
         }
         else if constexpr (requires { C1::HeapProvider; }) {
            static_assert(C1::HeapProvider == ACC,
               "Invalid heap provider ID");
            static_assert(not requires { C1::StackProvider; },
               "Component can't be both a stack and a heap provider");

            if constexpr (sizeof...(CN) > 0)
               return ValidateComponentOrderNested<ACC + 1, C1::ComponentPrecedence, CN...>();
            else
               return true;
         }
         else {
            if constexpr (sizeof...(CN) > 0)
               return ValidateComponentOrderNested<ACC, C1::ComponentPrecedence, CN...>();
            else {
               static_assert(ACC > 0,
                  "Container must have at least one heap or stack provider");
               return true;
            }
         }
      }

      /// Validate all used components in a container are properly ordered,   
      /// of standard layout, and containing proper sequential provider IDs.  
      ///   @tparam CN... components                                          
      template<class...CN>
      consteval bool ValidateComponentOrder() {
         static_assert(CT::Component<CN...>,
            "All elements must be components");
         static_assert(((not requires { typename CN::StateList; }) and ...),
            "The state component will be added automatically - remove it, and just "
            "rely on StateRequest(s) in other components.");
         static_assert((::std::is_standard_layout_v<CN> and ...),
            "All components must have standard layouts");
         static_assert((sizeof(CN) * ...) == 1,
            "Use StackRequest instead of adding non-static members to components");
         static_assert(sizeof...(CN) > 0,
            "Composed container must posses at least one heap/stack provider");
         if constexpr (sizeof...(CN) > 0)
            return ValidateComponentOrderNested<0, -1000000, CN...>();
         else
            return false;
      }

      /// std::tuple default-initializes variables to zero, so I use this     
      /// wrapper to get back to the biblically accurate behavior             
      template<class T>
      struct StackVariable {
         T value;

         /// Default initialization shouldn't initialize anything, but also   
         /// completely fail if T is a reference type.                        
         constexpr StackVariable() noexcept requires (CT::NotReference<T>) {};

         /// Constructs directly if possible                                  
         constexpr StackVariable(auto&& v) noexcept requires (    requires { T{LglsFwd(v)}; })
            : value {LglsFwd(v)} {}

         /// Strips intents before constructing in case first attempt         
         /// fails. Useful for primitive types that don't support intents.    
         constexpr StackVariable(auto&& v) noexcept requires (not requires { T{LglsFwd(v)}; })
            : value {LglsFwd(DeintCast(v))} {}
      };
      
      /// Go through all components and accumulate their stack requests into  
      /// a tuple                                                             
      template<class C1, class...CN>
      consteval auto DefineStack() {
         if constexpr (requires { typename C1::StackRequest; }) {
            if constexpr (CT::NotVoid<typename C1::StackRequest>) {
               Types<StackVariable<typename C1::StackRequest>> first;

               if constexpr (sizeof...(CN))
                  return first + DefineStack<CN...>();
               else
                  return first;
            }
            else {
               if constexpr (sizeof...(CN))
                  return DefineStack<CN...>();
               else
                  return NoTypes{};
            }
         }
         else {
            if constexpr (sizeof...(CN))
               return DefineStack<CN...>();
            else
               return NoTypes {};
         }
      }
      
      /// Go through all components until PICK is reached, and accumulate     
      /// the offset up to that point, to get the index in the stack tuple    
      template<class PICK, class C1, class...CN>
      consteval size_t GetStackOffset() {
         static_assert(requires { typename PICK::StackRequest; },
            "Component data is not on the stack");
          
         if constexpr (CT::DerivedFrom<C1, PICK>)
            return 0;
         else {
            size_t offset = 0;
            if constexpr (requires { typename C1::StackRequest; }) {
               if constexpr (CT::NotVoid<typename C1::StackRequest>)
                  ++offset;
            }
         
            if constexpr (sizeof...(CN))
               return offset + GetStackOffset<PICK, CN...>();
            else
               return offset;
         }
      }
      
      /// Go through all components and accumulate their heap requests into   
      /// a byte amount, used for header size when allocating                 
      ///   @return the size of the heap header in bytes                      
      template<Cid ID, class C1, class...CN>
      consteval size_t DefineHeapHeader() {
         if constexpr (requires { typename C1::HeapRequest; }) {
            size_t offset = 0;
            using R = typename C1::HeapRequest;
            if constexpr (requires { R::AllocatedPerIndirection; })
               ;
            else if constexpr (requires { R::AllocatedPerElement; })
               ;
            else if constexpr (C1::Id == ID)
               offset += sizeof(R);
            
            if constexpr (sizeof...(CN))
               return offset + DefineHeapHeader<ID, CN...>();
            else
               return offset;
         }
         else {
            if constexpr (sizeof...(CN))
               return DefineHeapHeader<ID, CN...>();
            else
               return 0;
         }
      }      
      
      /// Go through all components and accumulate their heap requests into   
      /// a byte amount, used for footer size when allocating                 
      ///   @param count footer can depend on the amount of elements          
      ///   @param indirects footer can depend on the indirections            
      ///   @return the size of the heap footer in bytes                      
      template<class C1, class...CN>
      constexpr size_t DefineHeapFooter(
         [[maybe_unused]] const size_t count,
         [[maybe_unused]] const size_t indirects
      ) noexcept {
         if constexpr (requires { typename C1::HeapRequest; }) {
            size_t offset = 0;
            using R = typename C1::HeapRequest;
            if constexpr (requires { R::AllocatedPerIndirection; }) {
               if constexpr (requires { R::Type::AllocatedPerElement; })
                  offset += sizeof(typename R::Type::Type) * count * indirects;
               else
                  offset += sizeof(typename R::Type) * indirects;
            }
            else if constexpr (requires { R::AllocatedPerElement; }) {
               if constexpr (requires { R::Type::AllocatedPerIndirection; })
                  offset += sizeof(typename R::Type::Type) * count * indirects;
               else
                  offset += sizeof(typename R::Type) * count;
            }
            
            if constexpr (sizeof...(CN))
               return offset + DefineHeapFooter<CN...>(count, indirects);
            else
               return offset;
         }
         else {
            if constexpr (sizeof...(CN))
               return DefineHeapFooter<CN...>(count, indirects);
            else
               return 0;
         }
      }
      
      /// Go through all components until PICK is reached, and accumulate     
      /// the offset up to that point, to get the byte offset in the header   
      ///   @return the header offset, where PICK's data resides              
      template<class PICK, class C1, class...CN>
      consteval size_t GetHeapHeaderOffset() {
         static_assert(requires { typename PICK::HeapRequest; },
            "Component data is not on the heap");
         static_assert(
                not requires { PICK::HeapRequest::AllocatedPerIndirection; }
            and not requires { PICK::HeapRequest::AllocatedPerElement; },
            "Component data doesn't reside in header, use GetHeapFooterOffset instead"
         );
          
         if constexpr (CT::DerivedFrom<C1, PICK>)
            return 0;
         else {
            size_t offset = 0;
            if constexpr (requires { typename C1::HeapRequest; }) {
               using R = typename C1::HeapRequest;
               if constexpr (requires { R::AllocatedPerIndirection; })
                  ;
               else if constexpr (requires { R::AllocatedPerElement; })
                  ;
               else offset += sizeof(R);
            }
         
            if constexpr (sizeof...(CN))
               return offset + GetHeapHeaderOffset<PICK, CN...>();
            else
               return offset;
         }
      }
      
      /// Go through all components until PICK is reached, and accumulate     
      /// the offset up to that point, to get the byte offset in the heap     
      ///   @param count heap requests can depend on the amount of elements   
      ///   @param indirects heap requests can depend on the indirections     
      ///   @return the heap byte offset, where PICK's data resides           
      template<class PICK, class C1, class...CN>
      constexpr size_t GetHeapFooterOffset(
         [[maybe_unused]] const size_t count,
         [[maybe_unused]] const size_t indirects
      ) noexcept {
         static_assert(requires { typename PICK::HeapRequest; },
            "Component data is not on the heap");
         static_assert(
               requires { PICK::HeapRequest::AllocatedPerIndirection; }
            or requires { PICK::HeapRequest::AllocatedPerElement; },
            "Component data doesn't reside in footer, use GetHeapHeaderOffset instead"
         );

         if constexpr (CT::DerivedFrom<C1, PICK>)
            return 0;
         else {
            size_t offset = 0;
            if constexpr (requires { typename C1::HeapRequest; }) {
               using R = typename C1::HeapRequest;
               if constexpr (requires { R::AllocatedPerIndirection; }) {
                  if constexpr (requires { R::Type::AllocatedPerElement; })
                     offset += sizeof(typename R::Type::Type) * count * indirects;
                  else
                     offset += sizeof(typename R::Type) * indirects;
               }
               else if constexpr (requires { R::AllocatedPerElement; }) {
                  if constexpr (requires { R::Type::AllocatedPerIndirection; })
                     offset += sizeof(typename R::Type::Type) * count * indirects;
                  else
                     offset += sizeof(typename R::Type) * count;
               }
            }
         
            if constexpr (sizeof...(CN))
               return offset + GetHeapFooterOffset<PICK, CN...>(count, indirects);
            else
               return offset;
         }
      }
   }

   template<class...CN>
   concept ValidComponentOrder = Inner::ValidateComponentOrder<CN...>();
}