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
   template<class T>
   struct State;
   
   /// Affects CT::Component<T>                                               
   template<class T>
   struct Component;
   
   /// Affects CT::Container<T>                                               
   template<class T>
   struct Container;
   
   /// Affects CT::Map<T>                                                     
   template<class T>
   struct Map;
   
   /// Affects CT::Set<T>                                                     
   template<class T>
   struct Set;
   
   /// Affects CT::Pair<T>                                                    
   template<class T>
   struct Pair;

   /// Affects CT::Handle<T>                                                  
   template<class T>
   struct Handle;

   /// Affects CT::Iterator<T>                                                
   template<class T>
   struct Iterator;
}

LANGULUS_CTTI_CONCEPT_DECVQ(State);
LANGULUS_CTTI_CONCEPT_DECVQ(Component);
LANGULUS_CTTI_CONCEPT_DECVQ(Container);
LANGULUS_CTTI_CONCEPT_DECVQ(Map);
LANGULUS_CTTI_CONCEPT_DECVQ(Set);
LANGULUS_CTTI_CONCEPT_DECVQ(Pair);
LANGULUS_CTTI_CONCEPT_DECVQ(Handle);
LANGULUS_CTTI_CONCEPT_DECVQ(Iterator);

namespace Langulus::CT
{
   /// Check if listed types are containers with any kind of Ownership        
   /// component                                                              
   template<class...T>
   concept Owned = Container<T...>
       and (ShedDeref<T>::Owned and ...);

   /// Check if listed containers are referenced upon construction/assignment 
   /// and then automatically dereferenced on destruction                     
   template<class...T>
   concept AutoOwned = Container<T...>
       and ((ShedDeref<T>::AutoOwned) and ...);
   
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

   /// A component ID                                                         
   using Cid = uint;

   /// A helper structure for pairing heap components with type components    
   ///   @tparam ID - the type component ID                                   
   ///   @tparam POINTER_TYPE - associated heap pointer type - mainly a       
   ///      customization point for packed pointer use.                       
   template<Cid ID = 0, CT::Sparse POINTER_TYPE = void*>
   struct HeapEntry {
      static constexpr Cid Id = ID;
      using T = POINTER_TYPE;
   };

   namespace State
   {
      enum StateValue {
         Variable = 0,
         Enabled = 1,
         Disabled = 2
      };
   }

   namespace DefineState
   {
      struct Default;
      template<State::StateValue = State::Variable> struct Compressed;
      template<State::StateValue = State::Variable> struct Encrypted;
      template<State::StateValue = State::Variable> struct Future;
      template<State::StateValue = State::Variable> struct Or;
      template<State::StateValue = State::Variable> struct Past;
      template<State::StateValue = State::Variable> struct Sorted;
      template<State::StateValue = State::Variable> struct Tracked;
      template<State::StateValue = State::Variable> struct Typed;
   }

   namespace Component
   {
      /// Components, predeclared                                             
      template<class META, class TYPE = void, bool CONSTRAIN = not ::std::is_void_v<TYPE>, Cid = 0> struct TypedStack;
      template<class META, CT::NotVoid TYPE, Cid = 0> struct TypedStatic;

      template<Cid = 0, uint = 0, uint = 0, CT::HeapEntry...> struct HeapImmovable;
      template<Cid = 0, uint = 0, uint = 0, CT::HeapEntry...> struct HeapMovable;
      template<Cid = 0, CT::HeapEntry...> struct HeapReference;
      template<CT::NotVoid, Cid = 0>      struct Stack;

      struct Charge;
      #define LglsComCharge(modifier) modifier struct Charge

      template<Cid = 0, bool HASH = true, Cid...> struct Comparison;
      #define LglsComComparison(modifier) template<Cid, bool, Cid...> modifier struct Comparison

      template<Cid = 0, Cid...>                       struct Conversion;

      template<Cid = 0, class T = size_t, Cid...>     struct CountHeap;
      template<Cid = 0, class T = size_t, Cid...>     struct CountStack;
      template<Cid = 0, auto COUNT = 0u, Cid...>      struct CountStatic;

      template<Cid = 0, class T = size_t, Cid...>     struct ReserveEmergent;
      template<Cid = 0, class T = size_t, Cid...>     struct ReserveStack;
      template<Cid = 0, auto COUNT = 0u, Cid...>      struct ReserveStatic;

      template<Cid = 0, bool AUTO = true, Cid...>     struct OwnershipEmergent;
      template<Cid = 0, bool AUTO = true, Cid...>     struct OwnershipStack;

      template<Cid = 0, bool REF_INDIVIDUAL = true>   struct OwnershipDeepEmergent;
      template<Cid = 0, bool REF_INDIVIDUAL = true>   struct OwnershipDeepHeap;
      template<Cid = 0, bool REF_INDIVIDUAL = true>   struct OwnershipDeepReference;

      template<Cid = 0, class H  = Hash, Cid...>      struct HashEmergent;
      template<Cid = 0, class H  = Hash, Cid...>      struct HashHeap;
      template<Cid = 0, class H  = Hash, Cid...>      struct HashStack;

      template<Cid = 0, Cid...>                       struct IndexedCommon;
      template<Cid = 0, Cid...>                       struct IndexedLinear;
      template<Cid = 0, class H  = Hash, Cid...>      struct IndexedCommonHashed;
      template<Cid = 0, class H  = Hash, Cid...>      struct IndexedHashHeap;
      template<Cid = 0, class H  = Hash, Cid...>      struct IndexedHashStack;

      template<Cid = 0, Cid...>           struct Emplacement;
      template<Cid = 0>                   struct Assignment;
      #define LglsComAssignment(modifier) template<Cid ID> modifier struct Assignment

      template<Cid = 0, class AS = void>  struct Insertion;
      template<Cid = 0, class AS = void>  struct InsertionOperators;
      template<Cid = 0, class AS = void, Cid...>  struct Merging;
      #define LglsComMerging(modifier) template<Cid, class, Cid...> modifier struct Merging

      template<Cid = 0, class AS = void>  struct MergingOperators;
      template<Cid = 0, Cid...>           struct Removal;

      template<Cid = 0, Cid...>           struct IterationForEach;
      template<Cid = 0, Cid...>           struct IterationRange;
      template<Cid = 0>                   struct IterationOperators;

      template<CT::State...>              struct StateHeap;
      template<CT::State...>              struct StateStack;
      template<CT::State...>              struct StateStatic;

                                          struct Descriptor;
      template<Cid = 0>                   struct Extrapolation;
      template<Cid = 0>                   struct Interpolation;
   }
   
   namespace Com = Component;

   namespace Inner
   {
      /// Validate all used components in a container are properly ordered,   
      /// of standard layout, and containing proper ID sequences.             
      ///   @tparam ACC accumulated number of stack/heap providers            
      ///   @tparam C1, C2, CN... components                                  
      template<uint ACC, class C1, class C2, class...CN>
      consteval bool ValidateComponentOrder() {
         static_assert(::std::is_standard_layout_v<C1>);
         static_assert(::std::is_standard_layout_v<C2>);

         static_assert(C1::ComponentPrecedence <= C2::ComponentPrecedence,
            "Wrong component order");
         static_assert(sizeof(C1) == 1 and sizeof(C2) == 1,
            "Use StackRequest instead of adding non-static members in components");
         
         if constexpr (requires { C1::StackProvider; }) {
            static_assert(C1::StackProvider == ACC,
               "Invalid stack provider ID");
            static_assert(not requires { C1::HeapProvider; }, 
               "Component can't be both a stack and a heap provider");

            if constexpr (sizeof...(CN))
               return ValidateComponentOrder<ACC + 1, C2, CN...>();
            else {
               static_assert(ACC > 0,
                  "Container must have at least one heap or stack provider");
               return true;
            }
         }
         else if constexpr (requires { C1::HeapProvider; }) {
            static_assert(C1::HeapProvider == ACC,
               "Invalid heap provider ID");
            static_assert(not requires { C1::StackProvider; },
               "Component can't be both a stack and a heap provider");

            if constexpr (sizeof...(CN))
               return ValidateComponentOrder<ACC + 1, C2, CN...>();
            else {
               static_assert(ACC > 0,
                  "Container must have at least one heap or stack provider");
               return true;
            }
         }
         else {
            if constexpr (sizeof...(CN))
               return ValidateComponentOrder<ACC, C2, CN...>();
            else {
               static_assert(ACC > 0,
                  "Container must have at least one heap or stack provider");
               return true;
            }
         }
      }

      /// std::tuple default-initializes variables to zero, so I use this     
      /// wrapper to get back to the biblically accurate behavior             
      template<class T>
      struct StackVariable {
         T value;
         constexpr StackVariable() noexcept {};
         constexpr StackVariable(T const& v) noexcept : value {v} {}
         constexpr StackVariable(T&& v) noexcept : value {LglsFwd(v)} {}
      };
      
      /// Go through all components and accumulate their stack requests into  
      /// a tuple                                                             
      template<class C1, class...CN>
      consteval auto DefineStack() {
         if constexpr (requires { typename C1::StackRequest; }) {
            if constexpr (sizeof...(CN))
               return decltype(Types<StackVariable<typename C1::StackRequest>>::Concat(DefineStack<CN...>())) {};
            else
               return Types<StackVariable<typename C1::StackRequest>> {};
         }
         else {
            if constexpr (sizeof...(CN))
               return DefineStack<CN...>();
            else
               return Types<>{};
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
            if constexpr (requires { typename C1::StackRequest; })
               ++offset;
         
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
}