///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds a variable to a container that only references a remote heap.     
   /// No allocation interface is provided.                                   
   /// Increases the container's bytesize.                                    
   ///   @tparam ID - multiple references are supported                       
   template<unsigned ID = 0>
   struct HeapReference {
      using CTTI_Component = Yes<>;
      static constexpr int  StackSize = sizeof(void*);
      static constexpr int  ComponentPrecedence = -2000;
      static constexpr bool HeapAllocated = true;
      static constexpr bool HeapCanBeNull = false;

   protected:
      template<unsigned>
      friend struct IterationOperators;
      template<unsigned>
      friend struct Removal;
      template<class>
      friend struct IndexedLinear;
      template<unsigned>
      friend struct HeapMovable;

      /*using Byte = ::std::uint8_t;
      template<CT::Container C>
      using View = typename C::ViewType;
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;
      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>, typename Deref<C>::PickMut, typename Deref<C>::Pick>;*/

      /*union {
         // The heap pointer in char form for easy debugging            
         char* mHeapReadable;
         // The heap pointer in a byte form for easy pointer arithmetics
         uint8_t* mHeap;
         // The heap pointer in a void form for easy static_cast        
         void* mHeapVoid;
      };*/
      
      /// Get the heap pointer (inner)                                        
      constexpr auto& GetHeapInner(this auto&& self) noexcept {
         using R = Tif<CT::Mutable<decltype(self)>, void*, void const*>;
         return *reinterpret_cast<R const*>(
            self.mStack + self.template StackOffset<HeapReference>
         );
      }

      constexpr void SetHeapInner(this auto& self, auto heap) noexcept {
         const_cast<void*&>(self.GetHeapInner()) = const_cast<void*>(
            static_cast<const void*>(heap)
         );
      }

   public:
      /// Check if the container has valid heap memory associated with it     
      bool IsAllocated(this auto const& self) noexcept {
         return self.GetHeapInner() != nullptr;
      }
      
      /// Get a direct access to the heap memory                              
      template<CT::Container C>
      constexpr auto GetRaw(this C&& self) noexcept {
         using T = TypeOf<C>;
         if constexpr (CT::Mutable<C>)
            return static_cast<      T*>(self.GetHeapInner());
         else
            return static_cast<const T*>(self.GetHeapInner());
      }
      
      /// Get a direct access to the heap memory as a different type          
      template<class T, CT::Container C>
      constexpr auto GetRawAs(this C&& self) noexcept {
         if constexpr (CT::Mutable<C>)
            return static_cast<      T*>(self.GetHeapInner());
         else
            return static_cast<const T*>(self.GetHeapInner());
      }

      /// Get a direct access to the heap memory's end                        
      /// Depends on the number of initialized elements                       
      template<CT::Container C>
      constexpr auto GetRawEnd(this C&& self) noexcept {
         if constexpr (CT::Typed<C>)
            return self.GetRaw() + self.GetCount();
         else
            return self.template GetRawAs<uint8_t>() + self.GetBytesize();
      }
    
      /// Get reference to first element as sparse or dense, depending on T.  
      /// This is a lower-level routine that does only sparseness checking.   
      /// No conversion or copying occurs, only pointer arithmetic.           
      ///   @attention assumes the container is typed                         
      ///   @attention assumes the container is allocated                     
      ///   @tparam T - the type of data we're accessing -                    
      ///      use void to use the type of the container, if statically typed 
      template<class T = void, CT::Container C>
      constexpr auto& Get(this C&& self) has_assumptions {
         static_assert(not CT::Handle<T>, "T can't be a handle");
         static_assert(not CT::Reference<T>, "Strip references first");
         using TT = Tif<CT::Void<T>, TypeOf<C>, T>;
         using TTC = Tif<CT::Mutable<C>, TT, TT const>;
         auto& mHeap = self.GetHeapInner();

         if constexpr (CT::Void<TT>) {
            // Type-erased reference, no casting                        
            if (self.IsSparse())
               return static_cast<void**&>(mHeap);
            return static_cast<void* &>(mHeap);
         }
         else if constexpr (Deref<C>::TypeErased) {
            // Casting to a desired runtime type                        
            LglsAssumeDev(self.IsTyped(), "Block is not typed");

            if (self.IsSparse()) {
               if constexpr (CT::Dense<TT>)
                  return **static_cast<TTC**>(mHeap);
               else
                  return  *static_cast<TTC* >(mHeap);
            }
            else {
               if constexpr (CT::Dense<TT>)
                  return *static_cast<TTC*>( mHeap);
               else
                  return *static_cast<TTC*>(&mHeap);
            }
         }
         else {
            // Casting to a desired static type                         
            if constexpr (Deref<C>::Sparse) {
               if constexpr (CT::Dense<TT>)
                  return **static_cast<TTC**>(mHeap);
               else
                  return  *static_cast<TTC* >(mHeap);
            }
            else {
               if constexpr (CT::Dense<TT>)
                  return *static_cast<TTC*>( mHeap);
               else
                  return *static_cast<TTC*>(&mHeap);
            }
         }
      }

      /// Get first element as a handle, or any desired wrapping type         
      ///   @attention assumes T is of proper sparseness if not void          
      ///   @tparam T - the type we're wrapping in                            
      template<class T, CT::Container C>
      T GetAs(this C&& self) has_assumptions {
         if constexpr (CT::Handle<T>) {
            static_assert(not CT::Reference<T>, "Strip references first");

            if constexpr (T::TypeErased) {
               // Type-erased handle                                    
               if constexpr (requires { T::Owned; }) {
                  if constexpr (T::Owned)
                     return {self.Get(), self.GetEntries(), self.GetType()};
                  else
                     return {self.Get(), self.GetType()};
               }
               else return {self.Get(), self.GetType()};
            }
            else {
               // Statically typed handle                               
               if constexpr (CT::Untyped<C>) {
                  LglsAssumeDev(self.template IsSimilar<TypeOf<T>>(),
                     "Sparseness mismatch");
               }

               if constexpr (requires { T::Owned; }) {
                  if constexpr (T::Owned)
                     return {&self.Get(), self.GetEntries()};
                  else
                     return {&self.Get()};
               }
               else return {&self.Get()};
            }
         }
         else return self.template Get<Deref<T>>();
      }

   protected:
      /// Default-initialize the component is impossible                      
      constexpr void ConstructDefault() {
         static_assert(false, "Can't default-construct this component");
      }
      
      /// Transfer from any kind of container.                                
      /// This is only a reference to a heap allocation and is not allowed    
      /// to allocate any new memory, so all this does is copy the heap       
      /// pointer, ignoring any intents.                                      
      ///   @param intent - the intent and container to transfer from         
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         self.SetHeapInner(intent.what.GetRaw());
      }
   };
}
