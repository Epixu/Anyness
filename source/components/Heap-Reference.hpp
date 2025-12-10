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
#include <Langulus/MetaOf.hpp>
#include <Langulus/Utils/Pot.hpp>


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
      
      /// Get the heap pointer (inner)                                        
      constexpr auto& GetHeapInner(this auto&& self) noexcept {
         return self.template AccessStack<HeapReference>();
      }

      /// Set the heap pointer, any data pointer will do                      
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
         using Tcvq = Tmut<C, TypeOf<C>*, TypeOf<C> const*>;
         return static_cast<Tcvq>(self.GetHeapInner());
      }
      
      /// Get a direct access to the heap memory as a different type          
      template<class T, CT::Container C>
      constexpr auto GetRawAs(this C&& self) noexcept {
         using Tcvq = Tmut<C, T*, T const*>;
         return static_cast<Tcvq>(self.GetHeapInner());
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
      constexpr decltype(auto) Get(this C&& self) has_assumptions {
         static_assert(not CT::Handle<T>,    "T can't be a handle");
         static_assert(not CT::Reference<T>, "Strip references first");
         using TC   = TypeOf<C>;
         using TH   = Tif<CT::Void<T>, TC, T>;
         using THQ1 = Tmut<C, TH*,  ConstAll<TH* >>;
         auto& mHeap = self.GetHeapInner();

         if constexpr (CT::Void<TH>) {
            // Unknown type, just return the heap pointer reference     
            return (mHeap);
         }
         else if constexpr (CT::TypeErased<C>) {
            // Casting to a desired runtime type                        
            LglsAssumeDev(self.IsTyped(), "Block is not typed");
            const auto indirections = self.GetIndirections();

            if (indirections == IndirectsOf<TH>) {
               // No difference in indirections                      
               return *static_cast<THQ1>(mHeap);
            }
            else if (indirections > IndirectsOf<TH>) {
               // We need to dereference                             
               auto diff = indirections - IndirectsOf<TH>;
               Deep<C> denser = Disown(self.GetDense(diff));
               return *static_cast<THQ1>(denser.GetHeapInner());
            }
            else {
               // We are allowed to add one additional indirection   
               LglsAssumeDev(indirections + 1 == IndirectsOf<TH>,
                  "Too many indirections");
               return *const_cast<THQ1>(reinterpret_cast<ConstAll<THQ1>>(&mHeap));
            }
         }
         else {
            // Casting to a desired static type                         
            if constexpr (IndirectsOf<TC> == IndirectsOf<TH>) {
               // No difference in indirections                         
               return *static_cast<THQ1>(static_cast<TC*>(mHeap));
            }
            else if constexpr (IndirectsOf<TC> > IndirectsOf<TH>) {
               // We need to dereference. Can be done without a         
               // reinterpret_cast, and thus be constexpr-friendly      
               return *static_cast<THQ1>(DenseCast<IndirectsOf<TC> - IndirectsOf<TH>>(static_cast<TC*>(mHeap)));
            }
            else {
               // We are allowed to add one additional indirection      
               static_assert(IndirectsOf<TC>+1 == IndirectsOf<TH>,
                  "Too many indirections");
               return *const_cast<THQ1>(reinterpret_cast<ConstAll<THQ1>>(&mHeap));
            }
         }
      }

      /// Get first element as a handle, or any desired wrapping type         
      ///   @tparam T - the type we're wrapping in                            
      ///   @return the element, as a reference if possible                   
      template<class T, CT::Container C>
      decltype(auto) As(this C&& self) {
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
               if constexpr (CT::TypeErased<C>) {
                  LglsAssert(self.template IsSame<HT>(), "Type mismatch",
                     ": ", self.GetType(), " not same as ", MetaDataOf<HT>());
               }
               else static_assert(Same<TypeOf<C>, HT>, "Type mismatch");

               if constexpr (CT::DeeplyOwned<T>)
                  return T {&self.Get(), self.GetEntries()};
               else if constexpr (CT::Owned<T>)
                  return T {&self.Get(), self.GetAllocation()};
               else
                  return T {&self.Get()};
            }
         }
         else {
            // Access directly                                          
            if constexpr (CT::TypeErased<C>) {
               LglsAssert(self.template Is<T>(), "Type mismatch",
                  ": ", self.GetType(), " not akin to ", MetaDataOf<T>());
            }
            else static_assert(Akin<TypeOf<C>, T>, "Type mismatch");
            return self.template Get<T>();
         }
      }

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

         if constexpr (CT::TypeErased<C>) {
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
      ///   @param self - deduced this                                        
      ///   @param count - how many levels of indirection to remove?          
      ///   @return the dense first element                                   
      template<class AS = void, CT::Container C>
      auto GetDense(this C&& self, Count<C> count = CountMax<C>) {
         using D = Tif<CT::Void<AS>, Deep<C>, AS>;
         using H = typename Decay<C>::HandleType;
         static_assert(CT::Container<D>,
            "D must result in a container type");
         static_assert(CT::HasVariableCount<D>,
            "D must allow for being empty");

         if (self.IsEmpty())
            return D {};
         if (not self.IsSparse() or count <= 0)
            return D {Piecewise, self.template As<H>()};

         // Check if origin type is complete before attempting anything 
         if constexpr (CT::TypeErased<C>) {
            const auto T = self.GetType();
            if (count >= T.GetIndirections()) {
               LglsAssert(T.GetOrigin(),
                  "Trying to interface incomplete data `", self.GetType(),
                  "` as dense"
               );
            }
         }
         else {
            using T = TypeOf<C>;
            if (count >= IndirectsOf<T>) {
               LglsAssert(CT::Complete<Decay<T>>,
                  "Trying to interface incomplete data `", self.GetType(),
                  "` as dense"
               );
            }
         }

         // Start iterating until dereferenced enough                   
         auto iterator = self.template As<H>();
         while (count and iterator.IsSparse()) {
            iterator.SetHeapInner(*static_cast<void const* const*>(iterator.GetHeapInner()));
            iterator.SetEntriesInner(iterator.GetEntriesInner() + 1);
            iterator.SetTypeInner(iterator.GetType().GetDeptr());
            --count;
         }

         return D {Piecewise, iterator};
      }

   protected:
      /// Default-initialization of this component is impossible              
      constexpr void ConstructDefault() const {
         static_assert(false, "Can't default-construct this component");
      }
      
      /// Transfer from any kind of container.                                
      /// This is only a reference to a heap allocation and is not allowed    
      /// to allocate any new memory, so all this does is copy the heap       
      /// pointer, ignoring any intents.                                      
      ///   @param self - deduced this                                        
      ///   @param intent - the intent and container to transfer from         
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         self.SetHeapInner(intent.what.GetRaw());
      }

      /// A simple request for allocating memory, which includes heap         
      /// byte size, number of reserved elements, and optional header offset. 
      struct Request {
         pot_t  mTotalBytes;
         size_t mHeaderBytes;
         size_t mReserved;
      };
      
      /// Get a size based on reflected allocation page and count             
      ///   @param self - deduced this                                        
      ///   @param count - the number of elements to request                  
      template<CT::Container C>
      Request RequestHeap(this C const& self, const size_t count) has_assumptions {
         Request result;
         const size_t header = self.GetHeapHeaderSize(count, self.GetIndirections());
         
         if constexpr (CT::TypeErased<C>) {
            const auto T = self.GetType();
            LglsAssumeDev(T, "Requesting allocation size for an untyped container");

            // Check for reflected minimal allocation at runtime        
            const auto size = T.GetSize();
            result.mHeaderBytes = Align(header, T.GetAlignment());
            result.mTotalBytes = Roof2(::std::max(
               count * size + result.mHeaderBytes,
               static_cast<size_t>(T.GetMinAllocation())
            ));
            result.mReserved = (result.mTotalBytes - result.mHeaderBytes) / size;
         }
         else {
            // Check for reflected minimal allocation at compile-time   
            using T = TypeOf<C>;

            result.mHeaderBytes = Align(header, alignof(T));
            result.mTotalBytes = Roof2(::std::max(
               count * sizeof(T) + result.mHeaderBytes,
               CT::GetMinAlloc<T>()
            ));
            result.mReserved = (result.mTotalBytes - result.mHeaderBytes) / sizeof(T);
         }

         return result;
      }
   };
}
