///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/IntentOf.hpp>
#include <Langulus/Sequence.hpp>
#include <Langulus/HashOf.hpp>
#include <Langulus/Utils/Tuple.hpp>
#include <Langulus/CT/Contiguous.hpp>

/// Make the rest of the code aware, that Langulus::Anyness has been included 
#define LANGULUS_LIBRARY_ANYNESS() 1
#define LANGULUS_ANYNESS_VERBOSITY_MASTER_SWITCH() 0

#define if_available(WHAT) if constexpr (requires { WHAT; }) { WHAT; }

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

namespace Langulus::Anyness
{
   /// Used for requesting dynamic data from the heap in container components 
   template<class T>
   struct PerElement {
      static constexpr bool AllocatedPerElement = true;
      using Type = T;
   };

   /// Used for requesting dynamic data from the heap in container components 
   template<class T>
   struct PerIndirection {
      static constexpr bool AllocatedPerIndirection = true;
      using Type = T;
   };

   namespace Component
   {
      /// Components predeclared                                              
      template<unsigned ID = 0> struct Assignment;
      struct Charge;
      template<unsigned ID = 0, bool HASH = true> struct Comparison;
      struct Conversion; template<unsigned ID = 0, class T = size_t>
      struct CountHeap; template<unsigned ID = 0, class T = size_t>
      struct CountStack;
      template<auto COUNT> struct CountStatic;
      template<unsigned ID = 0> struct OwnershipDeepHeap;
      template<unsigned ID = 0> struct OwnershipDeepStack;
      struct Descriptor;
      template<unsigned ID = 0> struct Emplacement;
      struct Extrapolation;
      template<unsigned ID, class H> struct HashEmergent;
      template<unsigned ID, class H> struct HashHeap;
      template<unsigned ID, class H> struct HashStack;
      template<unsigned ID = 0> struct HeapImmovable;
      template<unsigned ID = 0> struct HeapMovable;
      template<unsigned ID = 0, CT::Sparse POINTER_TYPE = void*> struct HeapReference;
      template<unsigned ID = 0, class HASH = Hash> struct IndexedHashHeap;
      template<unsigned ID = 0, class HASH = Hash> struct IndexedHashStack;
      template<unsigned ID = 0, class T = void> struct IndexedLinear;
      template<unsigned ID = 0, class AS = void> struct Insertion;
      template<unsigned ID = 0, class AS = void> struct InsertionOperators;
      template<unsigned ID = 0, class AS = void> struct Merging;
      template<unsigned ID = 0, class AS = void> struct MergingOperators;
      struct Interpolation;
      template<unsigned ID = 0> struct IterationForEach;
      template<unsigned ID = 0> struct IterationOperators;
      template<unsigned ID = 0> struct IterationRange;
      template<unsigned ID = 0, bool AUTO = true, bool DEEPREF = true> struct OwnershipEmergent;
      template<unsigned ID = 0, bool AUTO = true, bool DEEPREF = true> struct OwnershipStack;
      template<unsigned ID = 0> struct Removal;
      template<unsigned ID = 0, class T = size_t> struct ReserveEmergent;
      template<unsigned ID = 0, class T = size_t> struct ReserveStack;
      template<auto SIZE> struct ReserveStatic;
      template<CT::NotVoid, unsigned ID = 0> struct Stack;
      template<CT::State...> struct StateHeap;
      template<CT::State...> struct StateStack;
      template<CT::State...> struct StateStatic;
      template<class META, class TYPE = void, bool CONSTRAIN = not ::std::is_void_v<TYPE>, unsigned ID = 0> struct TypedStack;
      template<class META, CT::NotVoid TYPE,  unsigned ID = 0> struct TypedStatic;
   }
   
   struct Handle;
   struct HandleMut;
   struct HandleDisowned;
   struct HandleDisownedMut;
   template<class T> struct THandle;
   template<class T> struct THandleDisowned;

   namespace Com = Component;

   namespace Inner
   {
      /// Validate all used components in a container are properly ordered,   
      /// of standard layout, and containing proper ID sequences.             
      template<unsigned ACC, class C1, class C2, class...CN>
      consteval bool ValidateComponentOrder() {
         static_assert(::std::is_standard_layout_v<C1>);
         static_assert(::std::is_standard_layout_v<C2>);

         static_assert(C1::ComponentPrecedence <= C2::ComponentPrecedence,
            "Wrong component order");
         static_assert(sizeof(C1) == 1 and sizeof(C2) == 1,
            "Use StackRequest instead of adding non-static members in components");
         
         if constexpr (requires { C1::Id; }) {
            static_assert(C1::Id == ACC, "Invalid heap/stack ID");
            if constexpr (sizeof...(CN))
               return ValidateComponentOrder<ACC+1, C2, CN...>();
            else
               return true;
         }
         else {
            if constexpr (sizeof...(CN))
               return ValidateComponentOrder<ACC, C2, CN...>();
            else
               return true;
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
      ///   @param count heap requests can depend on the amount of elements   
      ///   @param indirects heap requests can depend on the indirections     
      ///   @return the size of the heap header in bytes                      
      template<class C1, class...CN>
      constexpr size_t DefineHeap(
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
            else offset += sizeof(R);
            
            if constexpr (sizeof...(CN))
               return offset + DefineHeap<CN...>(count, indirects);
            else
               return offset;
         }
         else {
            if constexpr (sizeof...(CN))
               return DefineHeap<CN...>(count, indirects);
            else
               return 0;
         }
      }
      
      /// Go through all components until PICK is reached, and accumulate     
      /// the offset up to that point, to get the byte offset in the heap     
      ///   @param count heap requests can depend on the amount of elements   
      ///   @param indirects heap requests can depend on the indirections     
      ///   @return the heap byte offset, where PICK's data resides           
      template<class PICK, class C1, class...CN>
      constexpr size_t GetHeapOffset(
         [[maybe_unused]] const size_t count,
         [[maybe_unused]] const size_t indirects
      ) noexcept {
         static_assert(requires { typename PICK::HeapRequest; },
            "Component data is not on the heap");
          
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
               else offset += sizeof(R);
            }
         
            if constexpr (sizeof...(CN))
               return offset + GetHeapOffset<PICK, CN...>(count, indirects);
            else
               return offset;
         }
      }
      
      /// Tag for calling container constructors that initalize the           
      /// internal stack tuple                                                
      struct Stackwise {};

      /// Tag for calling container constructors that emplaces elements       
      struct Piecewise {};

      /// Tag for calling container constructors that absorb container        
      struct Absorb {};
   }

   constexpr Inner::Stackwise Stackwise {};
   constexpr Inner::Piecewise Piecewise {};
   constexpr Inner::Absorb    Absorb {};
   
   ///                                                                        
   /// A container definition using composition                               
   ///   @tparam COMPONENTS list of components that define the container      
   ///      behavior. Order is verified based on ComponentPrecedence members  
   ///      for various reasons, the main ones being initialization order and 
   ///      build-time optimization: too many superficially different template
   ///      specializations will bloat code generation significantly and slow 
   ///      builds down a lot...                                              
   template<CT::Component...COMPONENTS>
   requires (Inner::ValidateComponentOrder<0, COMPONENTS...>())
   struct LANGULUS_EBCO Container : COMPONENTS... {
      using CTTI_Container = Yes<>;
      using ComponentList = Types<COMPONENTS...>;
      using Base = Container;

      /// Generate a new container type with additional components            
      ///   @attention doesn't check for duplicates                           
      template<CT::Component...MORE_COMPONENTS>
      using Include = Container<COMPONENTS..., MORE_COMPONENTS...>;

      /// Default constructor doesn't initialize anything.                    
      /// Your container needs to call ConstructDefault manually.             
      constexpr Container() noexcept = default;

      /// A tag-dispatch constructor that forwards arguments to mStack.       
      /// Used in some niche container cases, like TOwn.                      
      constexpr Container(Inner::Stackwise, auto&&...arguments)
         : mStack {LglsFwd(arguments)...} {}

      /// Default destructor does nothing. Each container has to implement    
      /// it, most likely by calling this->Destroy(). This is needed, because 
      /// the destructor relies on properly deducing 'this'.                  
      constexpr ~Container() noexcept = default;
      
      /// Check if container is valid                                         
      constexpr bool IsValid() const noexcept {
         if (this->GetCount() > 0)
            return true;

         bool for_other_reasons = false;
         ComponentList::ForEach([this, &for_other_reasons]<class C>{
            if_available(for_other_reasons |= this->C::IsValid());
         });
         return for_other_reasons;
      }
      
      /// Check if a component is included at compile-time                    
      template<class C>
      static constexpr bool HasComponent = AkinAsOneOf<C, COMPONENTS...>;

   protected:
      template<unsigned>               friend struct Com::IterationOperators;
      template<class, class, bool, unsigned> friend struct Com::TypedStack;
      template<CT::NotVoid, unsigned>  friend struct Com::Stack;
      template<unsigned, CT::Sparse>   friend struct Com::HeapReference;
      template<unsigned>               friend struct Com::HeapMovable;
      template<unsigned, bool, bool>   friend struct Com::OwnershipStack;
      template<unsigned>               friend struct Com::OwnershipDeepStack;
      template<unsigned>               friend struct Com::OwnershipDeepHeap;
      template<unsigned, class>        friend struct Com::CountStack;
      template<unsigned, class>        friend struct Com::HashStack;
      template<unsigned, bool>         friend struct Com::Comparison;
      template<unsigned>               friend struct Com::Assignment;
      template<CT::State...>           friend struct Com::StateStack;

      // Here lies the stack. It is an optimized tuple that is filled   
      // with requests from components.                                 
      typename decltype(Inner::DefineStack<COMPONENTS...>())::TupleOptimized mStack;

      /// Access a variable on the stack associated with a component          
      template<class COM, class SELF>
      constexpr auto& AccessStack(this SELF&& self) noexcept {
         constexpr size_t IDX = Inner::GetStackOffset<COM, COMPONENTS...>();
         auto& result = ::Langulus::get<IDX>(self.mStack).value;
         using ConstOrNot = LglsMutIf(SELF, decltype(result));
         return const_cast<ConstOrNot>(result);
      }

      /// Access a variable on the heap associated with a component           
      template<CT::Component COM, CT::Container SELF>
      constexpr auto AccessHeap(this SELF&& self) noexcept {
         size_t offset = Inner::GetHeapOffset<COM, COMPONENTS...>(
            static_cast<size_t>(self.GetReserved()),
            static_cast<size_t>(self.GetIndirections())
         );

         auto heap = self.GetAllocationInner()->GetBlockStart() + offset;
         using R = typename COM::HeapRequest;
         if constexpr (requires { R::AllocatedPerIndirection; }) {
            if constexpr (requires { R::Type::AllocatedPerElement; }) {
               using RC = LglsMutIf(SELF, typename R::Type::Type*);
               return reinterpret_cast<RC>(heap);
            }
            else {
               using RC = LglsMutIf(SELF, typename R::Type*);
               return reinterpret_cast<RC>(heap);
            }
         }
         else if constexpr (requires { R::AllocatedPerElement; }) {
            if constexpr (requires { R::Type::AllocatedPerIndirection; }) {
               using RC = LglsMutIf(SELF, typename R::Type::Type*);
               return reinterpret_cast<RC>(heap);
            }
            else {
               using RC = LglsMutIf(SELF, typename R::Type*);
               return reinterpret_cast<RC>(heap);
            }
         }
         else {
            using RC = LglsMutIf(SELF, R*);
            return reinterpret_cast<RC>(heap);
         }
      }
      
      /// Calculate the heap header size                                      
      static constexpr size_t GetHeapHeaderSize(size_t count, size_t indirects) noexcept {
         return Inner::DefineHeap<COMPONENTS...>(count, indirects);
      }

      /// Access a variable on the stack associated with an ID                
      template<unsigned ID>
      constexpr auto& AccessStackById(this auto&& self) noexcept {
         return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) {
            if constexpr (requires { C::Id; }) {
               if constexpr (C::Id == ID)
                  return (self.template AccessStack<C>());
               else return No {};
            }
            else return No {};
         });
      }

      /// Access a component on the stack associated with an ID               
      template<unsigned ID>
      constexpr auto& AccessComById(this auto&& self) noexcept {
         return ComponentList::ForEachConstOr([&]<class C>{
            if constexpr (C::Id == ID)
               return (self.C);
            else return No {};
         });
      }

      /// Explicitly call ConstructDefault in all of the components.          
      constexpr void ConstructDefault(this auto& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::ConstructDefault());
         });
      }
      
      /// Call ConstructFrom whenever possible, fallback to                   
      /// ConstructDefault otherwise                                          
      template<CT::Container SELF, CT::Container FROM>
      constexpr void Absorb(this SELF& self, FROM&& from) {
         static_assert(CT::Handle<FROM> == CT::Handle<SELF>,
            "Handles can't be absorbed into non-handles, use insertion instead");
         ComponentList::ForEach([&]<class C>{
                 if_available(self.C::ConstructFrom(FWDIntent(from)))
            else if_available(self.C::ConstructDefault())
         });
      }

      /// Call Destroy whenever possible                                      
      constexpr void Destroy(this auto& self) {
         if not consteval {
            ComponentList::ForEach([&]<class C> {
               if_available(self.C::Destroy());
            });
         }
      }

      /// Explicitly call AssignDefault in all of the components.             
      constexpr auto& AssignDefault(this auto& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::AssignDefault());
         });
         return self;
      }

   public:
      /// Call AssignFrom whenever possible, fallback to AssignDefault        
      /// otherwise                                                           
      template<CT::Container SELF, CT::Container FROM>
      constexpr SELF& AssignAbsorb(this SELF& self, FROM&& rhs) {
         static_assert(CT::Handle<FROM> == CT::Handle<SELF>,
            "Handles can't be absorbed into non-handles, use insertion instead");
         ComponentList::ForEach([&]<class C>{
                 if_available(self.C::AssignFrom(FWDIntent(rhs)))
            else if_available(self.C::AssignDefault())
         });
         return self;
      }
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
}

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
       and (ShedDeref<T>::HeapAllocated and ...);
   
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
}

namespace Langulus
{
   /// Loop controls from inside ForEach lambdas when iterating containers    
   struct LoopControl {
      enum Command : int {
         Break = 0,     // Break the loop                               
         Continue = 1,  // Continue the loop                            
         Repeat = 2,    // Repeat the current element                   
         Discard = 3,   // Remove the current element                   
         NextLoop = 4   // Skip to next function in the ForEach         
      } mControl;

      LoopControl() = delete;

      constexpr LoopControl(bool a) noexcept
         : mControl {static_cast<Command>(a)} {}
      constexpr LoopControl(Command a) noexcept
         : mControl {a} {}

      explicit constexpr operator bool() const noexcept {
         return mControl == Continue or mControl == Repeat;
      }

      constexpr bool operator == (const LoopControl&) const noexcept = default;
   };

   namespace Loop
   {
      /// Break the entire iteration as a whole                               
      constexpr LoopControl Break    = LoopControl::Break;
      /// Continue to next element or function                                
      constexpr LoopControl Continue = LoopControl::Continue;
      /// Repeat the current element                                          
      constexpr LoopControl Repeat   = LoopControl::Repeat;
      /// Remove the current element                                          
      constexpr LoopControl Discard  = LoopControl::Discard;
      /// End this iterating function and jump immediately to the next        
      constexpr LoopControl NextLoop = LoopControl::NextLoop;
   }
}

namespace Langulus::Anyness
{
   namespace Inner
   {
      /// Inner function that picks the best possible handle type, depending  
      /// on a container's constness and type-erasedness.                     
      template<CT::Container C> 
      consteval auto DecideHandleType() {
         static_assert(not CT::Sheddable<C>, "Strip sheddables first");
         static_assert(not CT::Reference<C>, "Strip references first");

         if constexpr (CT::TypeErased<C>) {
            // Type-erased handle                                       
            if constexpr (CT::Owned<C>)
               return Types<Tmut<C, HandleMut,         Handle>> {};
            else
               return Types<Tmut<C, HandleDisownedMut, HandleDisowned>> {};
         }
         else {
            // Statically-typed handle                                  
            using T = TypeOf<C>;
            if constexpr (CT::Owned<C>)
               return Types<THandle        <Tmut<C, T&, ConstAll<T&>>>> {};
            else
               return Types<THandleDisowned<Tmut<C, T&, ConstAll<T&>>>> {};
         }
      }
   }

   template<CT::Container C>
   using DecideHandle = typename decltype(Inner::DecideHandleType<C>())::First;
}
