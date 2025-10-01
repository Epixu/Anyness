///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Resolvable.hpp>
#include <Langulus/CT/MinAlloc.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds a variable to a container that only references a remote heap.     
   /// No allocation interface is provided.                                   
   /// Increases the container's bytesize.                                    
   ///   @tparam ID - multiple references are supported                       
   template<unsigned ID>
   struct HeapReference {
      using CTTI_Component = Yes<>;
      using StackRequest = void*;

      static constexpr unsigned Id = ID;
      static constexpr int  ComponentPrecedence = -2000;
      static constexpr bool HeapAllocated = true;
      static constexpr bool HeapCanBeNull = false;

   protected:
      template<unsigned>         friend struct IterationOperators;
      template<unsigned>         friend struct Removal;
      template<unsigned, class>  friend struct IndexedLinear;
      template<unsigned>         friend struct HeapMovable;
      template<unsigned>         friend struct Emplacement;
      template<unsigned, bool>   friend struct Comparison;
      template<auto COUNT>       friend struct CountStatic;
      
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;

      /*using Byte = ::std::uint8_t;
      template<CT::Container C>
      using View = typename C::ViewType;
      template<CT::Container C>
      using Pick = Tmut<C, typename Deref<C>::PickMut, typename Deref<C>::Pick>;*/

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
         return self.template AccessStack<HeapReference>();
      }

      constexpr void SetHeapInner(this auto& self, auto heap) noexcept {
         self.GetHeapInner() = const_cast<void*>(
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
            return static_cast<T*      >(self.GetHeapInner());
         else
            return static_cast<T const*>(self.GetHeapInner());
      }
      
      /// Get a direct access to the heap memory as a different type          
      template<class T, CT::Container C>
      constexpr auto GetRawAs(this C&& self) noexcept {
         if constexpr (CT::Mutable<C>)
            return static_cast<T*      >(self.GetHeapInner());
         else
            return static_cast<T const*>(self.GetHeapInner());
      }

      /// Get a direct access to the heap memory's end.                       
      /// Depends on the number of initialized elements.                      
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
      ///   @attention no type-safety                                         
      ///   @attention assumes the container is typed                         
      ///   @attention assumes the container is allocated                     
      ///   @tparam T - the type of data we're accessing -                    
      ///      use void to use the type of the container, if statically typed 
      template<class T = void, CT::Container C>
      constexpr /*decltype(auto)*/ auto& Get(this C&& self) has_assumptions {
         static_assert(not CT::Handle<T>,    "T can't be a handle");
         static_assert(not CT::Reference<T>, "Strip references first");
         using TC = TypeOf<C>;
         using TH = Tif<CT::Void<T>, TC, T>;
         using THQ1 = Tmut<C, TH*,  ConstAll<TH* >>;
         using THQ2 = Tmut<C, TH**, ConstAll<TH**>>;
         auto /*Tmut<C, void*, void const* const>*/& mHeap = self.GetHeapInner();

         if constexpr (CT::Void<TH>) {
            // Unknown type, just return the heap pointer reference     
            return (mHeap);
         }
         else if constexpr (Deref<C>::TypeErased) {
            // Casting to a desired runtime type                        
            LglsAssumeDev(self.IsTyped(), "Block is not typed");

            if (self.IsSparse()) {
               if constexpr (CT::Dense<TH>)
                  // Representing sparse as dense                       
                  return **static_cast<THQ2>(mHeap);
               else
                  // Representing sparse as sparse                      
                  return  *static_cast<THQ1>(mHeap);
            }
            else {
               if constexpr (CT::Dense<TH>)
                  // Representing dense as dense                        
                  return *static_cast<THQ1>( mHeap);
               else
                  // Representing dense as sparse                       
                  return *const_cast<THQ1>(reinterpret_cast<ConstAll<THQ1>>(&mHeap));
            }
         }
         else {
            // Casting to a desired static type                         
            if constexpr (CT::Sparse<TC>) {
               if constexpr (CT::Dense<TH>)
                  // Representing sparse as dense                       
                  return **static_cast<THQ2>(mHeap);
               else
                  // Representing sparse as sparse                      
                  return  *static_cast<THQ1>(mHeap);
            }
            else {
               if constexpr (CT::Dense<TH>)
                  // Representing dense as dense                        
                  return *static_cast<THQ1>( mHeap);
               else
                  // Representing dense as sparse                       
                  return *const_cast<THQ1>(reinterpret_cast<ConstAll<THQ1>>(&mHeap));
                  //return static_cast<Deptr<THQ1>>(mHeap);
            }
         }
      }

      /// Get first element as a handle, or any desired wrapping type         
      ///   @tparam T - the type we're wrapping in                            
      ///   @return the element, as a reference if possible                   
      template<class T, CT::Container C>
      decltype(auto) As(this C&& self) has_assumptions {
         static_assert(not CT::Reference<T>, "Strip references first");

         if constexpr (CT::Handle<T>) {
            if constexpr (CT::TypeErased<T>) {
               // Type-erased handle                                    
               if constexpr (CT::DeeplyOwned<T>)
                  return T {self.Get(), self.GetEntries(), self.GetType()};
               else if constexpr (CT::Owned<T>)
                  return T {self.Get(), self.GetAllocation(), self.GetType()};
               else
                  return T {self.Get(), self.GetType()};
            }
            else {
               // Statically typed handle                               
               using HT = Deref<TypeOf<T>>;
               if constexpr (CT::TypeErased<C>)
                  LglsAssumeDev(self.template IsSimilar<HT>(), "Type mismatch");
               else
                  static_assert(CT::Similar<TypeOf<C>, HT>, "Type mismatch");

               if constexpr (CT::DeeplyOwned<T>)
                  return T {self.HeapReference::template Get<HT*>(), self.GetEntries()};
               if constexpr (CT::Owned<T>)
                  return T {self.HeapReference::template Get<HT*>(), self.GetAllocation()};
               else
                  return T {self.HeapReference::template Get<HT*>()};
            }
         }
         else {
            // Access directly                                          
            if constexpr (CT::TypeErased<C>)
               LglsAssumeDev(self.template Is<T>(), "Type mismatch");
            else
               static_assert(CT::Same<TypeOf<C>, T>, "Type mismatch");

            return self.template Get<T>();
         }
      }
      
      /// Get first element by casting it to any desirable compatible type    
      ///   @tparam AS - the type we're casting to                            
      ///   @return the resulting value                                       
      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      AS Cast(this C const&);

      /// A safe way to get the first deep entry                              
      ///   @attention ignores sparseness                                     
      ///   @return a pointer to the first deep item, or nullptr if not deep  
      template<class AS = void, CT::Container C>
      auto GetDeep(this C&& self) noexcept {
         using D = Tif<CT::Void<AS>,
            Tmut<C, Deep<C>*, Deep<C> const*>,
            Tmut<C, AS*,      AS const*>
         >;

         if (self.IsEmpty() or not self.IsDeep())
            return D {nullptr};
         return self.template As<D>();
      }

      /// A safe way to get the first sparse entry after being resolved to    
      /// the most concrete type.                                             
      ///   @return the most concrete representation of the first item        
      template<class AS = void, CT::Container C>
      auto GetResolved(this C&& self) {
         using D = Tif<CT::Void<AS>, Deep<C>, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         static_assert(CT::HasVariableCount<D>, "D must allow for being empty");

         if (self.IsEmpty())
            return D {};
         if (not self.IsSparse())
            return self.template GetItem<D>();

         if constexpr (C::TypeErased) {
            const auto T = self.GetType();
            const auto resolver = T.GetResolver();
            if (resolver)
               return D {resolver(self.GetDense().GetRaw())};
            else
               return self.template GetDense<D>();

         }
         else {
            using T = TypeOf<C>;
            if constexpr (CT::Resolvable<T>)
               return D {DenseCast(self.template Get<T>()).GetResolved()};
            else
               return D {DenseCast(self.template Get<T>())};
         }
      }

      /// Get the first contained element, removing 'count' indirections      
      ///   @attention throws if type is incomplete and origin was reached    
      ///   @param count - how many levels of indirection to remove?          
      ///   @return the dense first element                                   
      template<class AS = void, CT::Container C>
      auto GetDense(this C&& self, Count<C> const count = CountMax<C>) {
         using D = Tif<CT::Void<AS>, Deep<C>, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         static_assert(CT::HasVariableCount<D>, "D must allow for being empty");

         if (self.IsEmpty())
            return D {};
         if (self.IsDense() or count <= 0)
            return self.template GetItem<D>();

         // Check if origin type is complete before attempting anything 
         if constexpr (CT::TypeErased<C>) {
            const auto T = self.GetType();
            if (count >= T.GetIndirections()) {
               LglsAssert(T.GetOrigin(),
                  "Trying to interface incomplete data `", self.GetType(), "` as dense");
            }
         }
         else {
            using T = TypeOf<C>;
            if (count >= IndirectsOf<T>) {
               LglsAssert(CT::Complete<Decay<T>>,
                  "Trying to interface incomplete data `", self.GetType(), "` as dense");
            }
         }

         // Start iterating until dense                                 
         auto counter = count;
         auto first = self.GetItem();
         constexpr bool first_was_referenced = CT::AutoOwned<decltype(first)>;

         while (counter and first.IsSparse()) {
            auto& a = first.GetAllocationInner();
            if constexpr (first_was_referenced)
               if (a) a->Free();

            first.SetHeapInner(*static_cast<void const* const*>(first.GetHeapInner()));
            first.SetTypeInner(first.GetType().GetDeptr());

            const auto entries = first.GetEntries();
            if (entries) {
               a = *entries;
               if constexpr (first_was_referenced)
                  if (a) a->Keep();
            }
            else first.SetAllocationInner(nullptr);

            --counter;
         }

         return D {Abandon {first}};
      }

   protected:
      /// Default-initialization of this component is impossible              
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

      /// A simple request for allocating memory, which includes heap         
      /// byte size, number of reserved elements, and optional header offset. 
      struct Request {
         size_t mTotalBytes   IF_SAFE(= 0);
         size_t mHeaderBytes  IF_SAFE(= 0);
         size_t mReserved     IF_SAFE(= 0);
         IF_UNSAFE(constexpr Request() {})
      };
      
      /// Get a size based on reflected allocation page and count             
      ///   @param count - the number of elements to request                  
      template<CT::Container C>
      auto RequestHeap(this C const& self, const size_t count) has_assumptions -> Request {
         Request result;
         const size_t header = self.GetHeapHeaderSize();
         
         if constexpr (CT::TypeErased<C>) {
            const auto T = self.GetType();
            LglsAssumeDev(T, "Requesting allocation size for an untyped container");

            // Check for reflected minimal allocation at runtime        
            const auto size = T.GetSize();
            result.mHeaderBytes = Align(header, T.GetAlignment());
            result.mTotalBytes = Roof2(::std::max(count * size + result.mHeaderBytes, T.GetMinAllocation()));
            result.mReserved = (result.mTotalBytes - result.mHeaderBytes) / size;
         }
         else {
            // Check for reflected minimal allocation at compile-time   
            using T = TypeOf<C>;

            result.mHeaderBytes = Align(header, alignof(T));
            result.mTotalBytes = Roof2(::std::max(count * sizeof(T) + result.mHeaderBytes, CT::GetMinAlloc<T>()));
            result.mReserved = (result.mTotalBytes - result.mHeaderBytes) / sizeof(T);
         }

         return result;
      }
   };
}
