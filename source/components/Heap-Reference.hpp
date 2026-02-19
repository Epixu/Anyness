///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "Iteration-Range.hpp"
#include <Langulus/CT/Resolvable.hpp>
#include <Langulus/CT/MinAlloc.hpp>
#include <Langulus/MetaOf.hpp>
#include <Langulus/Utils/Pot.hpp>
#include <Langulus/Allocator.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds a variable to a container that only references a remote heap.     
   /// No allocation interface is provided.                                   
   /// Increases the container's bytesize.                                    
   ///   @tparam ID heap's unique identifier                                  
   ///   @tparam POINTER_TYPE heap pointer type (you can use packed pointers) 
   template<unsigned ID, CT::Sparse POINTER_TYPE>
   struct HeapReference {
      using CTTI_Component = Yes<>;
      using StackRequest = POINTER_TYPE;

      static constexpr unsigned Id = ID;
      static constexpr unsigned HeapProvider = ID;
      static constexpr int  ComponentPrecedence = -2000;
      static constexpr bool HeapCanBeNull = false;

   protected:

      template<unsigned>             friend struct IterationOperators;
      template<unsigned>             friend struct Removal;
      template<unsigned, class>      friend struct IndexedLinear;
      template<unsigned, CT::Sparse> friend struct HeapMovable;
      template<unsigned>             friend struct Emplacement;
      template<unsigned, bool>       friend struct Comparison;
                                     friend struct Conversion;
      template<auto COUNT>           friend struct CountStatic;
      template<unsigned, bool, bool> friend struct OwnershipEmergent;
      template<unsigned>             friend struct OwnershipDeepEmergent;
      
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      //template<CT::Container C>
      //using Deep = typename Deref<C>::DeepType;
      
      /// Get the heap pointer (inner)                                        
      constexpr auto& GetHeapInner(this auto&& self) noexcept {
         return self.template AccessStack<HeapReference>();
      }
      /// Get the heap pointer as a void* (inner)                             
      constexpr void* GetHeapInnerAsVoid(this auto&& self) noexcept {
         return static_cast<void*>(const_cast<DecvqAll<POINTER_TYPE>>(self.GetHeapInner()));
      }

      /// Set the heap pointer, any data pointer will do                      
      template<CT::Sparse P>
      /*constexpr*/ void SetHeapInner(this auto& self, P heap) assumptious { //can't be constexpr due to GCC ICE
         if constexpr (CT::CustomPointer<P>)
            self.GetHeapInner() = static_cast<POINTER_TYPE>(heap.Unpack());
         else
            self.GetHeapInner() = static_cast<POINTER_TYPE>(DecvqAllCast(heap));
      }

      constexpr void SetHeapInner(this auto& self, nullptr_t) noexcept {
         self.GetHeapInner() = nullptr;
      }

   public:
      /// Get a direct access to the heap memory                              
      ///   @attention accessing this while GetCount() is zero is undefined   
      template<CT::Container C>
      constexpr auto GetRaw(this C&& self) noexcept {
         using Tcvq = LglsMutIf(C, POINTER_TYPE);
         return static_cast<Tcvq>(self.GetHeapInner());
      }
      
      /// Get a direct access to the heap memory as a different type          
      ///   @attention accessing this while GetCount() is zero is undefined   
      template<class T, CT::Container C>
      constexpr auto GetRawAs(this C&& self) noexcept {
         using Tcvq = LglsMutIf(C, T*);
         return static_cast<Tcvq>(self.GetHeapInnerAsVoid());
      }

      /// Get a direct access to the heap memory's end.                       
      /// Depends on the number of initialized elements.                      
      ///   @attention accessing this while GetCount() is zero is undefined   
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
      ///   @attention assumes the container has valid heap                   
      ///   @tparam AS the type of data we're accessing - use void to use the 
      ///      type of the container, if statically typed                     
      template<class AS = void, CT::Container C> /*requires CT::Contiguous<C>*/
      constexpr decltype(auto) Get(this C&& self) assumptious {
         static_assert(not CT::Handle<AS>,    "AS can't be a handle");
         static_assert(not CT::Reference<AS>, "Strip references first");
         using TC   = LglsMutIf(C, TypeOf<C>);
         using TCP  = LglsMutIf(C, TC*);
         using TH   = Tif<CT::Void<AS>, TC, AS>;
         using THP  = LglsMutIf(C, TH*);
         auto& heap = self.GetHeapInner();

         if constexpr (CT::TypeErased<C>) {
            if constexpr (CT::Void<AS>) {
               // Unknown type, just return the heap pointer reference  
               return (heap);
            }
            else {
               // Casting to a desired runtime type                     
               LglsAssumeDev(self.IsTyped(), "Block is not typed");
               const auto indirections = self.GetIndirections();

               if (indirections == IndirectsOf<TH>) {
                  // No difference in indirections                      
                  return *static_cast<THP>(heap);
               }
               else if (indirections > IndirectsOf<TH>) {
                  // We need to dereference. Supports packed pointers.  
                  auto diff = indirections - IndirectsOf<TH>;
                  using Deep = typename Deref<C>::DeepType;
                  Deep denser = Disown(self.GetDense(diff));
                  return *static_cast<THP>(denser.GetHeapInner());
               }
               else {
                  // We are allowed to add one additional indirection   
                  LglsAssumeDev(indirections + 1 == IndirectsOf<TH>,
                     "Too many indirections");
                  return *const_cast<THP>(reinterpret_cast<ConstAll<THP>>(&heap));
               }
            }
         }
         else {
            // Casting to a desired static type                         
            if constexpr (IndirectsOf<TC> == IndirectsOf<TH>) {
               // No difference in indirections                         
               return *static_cast<THP>(static_cast<TCP>(heap));
            }
            else if constexpr (IndirectsOf<TC> > IndirectsOf<TH>) {
               // We need to dereference. Can be done without a         
               // reinterpret_cast, and thus be constexpr-friendly.     
               // Supports packed pointers as well.                     
               return *static_cast<THP>(DenseCast<IndirectsOf<TC> - IndirectsOf<TH>>(static_cast<TCP>(heap)));
            }
            else {
               // We are allowed to add one additional indirection      
               static_assert(IndirectsOf<TCP> == IndirectsOf<TH>,
                  "Too many indirections");
               return *const_cast<THP>(reinterpret_cast<ConstAll<THP>>(&heap));
            }
         }
      }

      /// Get first element as a handle, or any desired wrapping type         
      ///   @tparam T the type we're wrapping in                              
      ///   @return the element, as a reference if possible                   
      template<CT::NotVoid T, CT::Container C> requires CT::Contiguous<C>
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

      /// A safe way to get the first deep entry.                             
      /// Available only if container has DeepType defined.                   
      ///   @attention ignores sparseness                                     
      ///   @return a pointer to the first deep item, or nullptr if not deep  
      /*template<class AS = void, CT::Container C> requires CT::Contiguous<C>
      auto GetDeep(this C&& self) noexcept
      requires requires { typename Deref<C>::DeepType; } {
         using D = Tif<CT::Void<AS>, LglsMutIf(C, typename Deref<C>::DeepType*), LglsMutIf(C, AS*)>;
         if (self.IsEmpty() or not self.IsDeep())
            return D {nullptr};
         return self.template As<D>();
      }*/

      /// A safe way to get the first sparse entry after being resolved to    
      /// the most concrete type. Available only if container has DeepType.   
      ///   @return the most concrete representation of the first item        
      template<class AS = void, CT::Container C> requires CT::Contiguous<C>
      auto GetResolved(this C&& self)
      requires requires { typename Deref<C>::DeepType; } {
         using D = Tif<CT::Void<AS>, typename Deref<C>::DeepType, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         static_assert(CT::HasVariableCount<D>, "D must allow for being empty");

         if (self.IsEmpty())
            return D {};
         if (not self.IsSparse())
            return self.template As<D>();

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

      /// Get the first contained element, removing 'count' indirections.     
      /// Available only if container has DeepType defined.                   
      ///   @attention throws if type is incomplete and origin was reached    
      ///   @tparam AS specify the type we wrap the result in.                
      ///      Using 'void' will choose C::DeepType.                          
      ///   @param self deduced this                                          
      ///   @param count how many levels of indirection to remove?            
      ///   @return the dense first element                                   
      template<class AS = void, CT::Container C> requires CT::Contiguous<C>
      auto GetDense(this C&& self, Count<C> count = CountMax<C>)
      requires requires { typename Deref<C>::DeepType; } {
         using D = Tif<CT::Void<AS>, typename Deref<C>::DeepType, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         LglsAssert(not self.IsEmpty(), "Can't GetDense from empty container");
         if (not self.IsSparse() or count <= 0)
            return D {Absorb, Disown(self)};

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

         void* src = DecvqAllCast(self.GetHeapInner());
         auto T = self.GetType();
         while (count and T.IsSparse()) {
            auto nextT = T.GetDeptr();
            
            if (nextT.IsSparse()) {
               // Pointer T -> Pointer nextT                            
               T.GetDereffer()(src, &src);
            }
            else {
               // Pointer T -> Dense nextT                              
               D temp {Absorb, Disown(self)};
               temp.SetTypeInner(nextT);
               temp.SetHeapInner(UnpackPointer(T, nextT, src));
               if_available(temp.SetCountInner(1));
               return temp;
            }

            T = nextT;
            --count;
         }
         
         LglsError("Should never be reached");
         return D {Absorb, Disown(self)};
      }

   protected:
      /// Default-initialization of this component                            
      void ConstructDefault(this auto& self) noexcept {
         self.SetHeapInner(nullptr);
      }
      
      /// Transfer from any kind of container.                                
      /// This is only a reference to a heap allocation and is not allowed    
      /// to allocate any new memory, so all this does is copy the heap       
      /// pointer, ignoring any intents.                                      
      ///   @param self deduced this                                          
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) noexcept {
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
      ///   @param self deduced this                                          
      ///   @param count the number of elements to request                    
      template<CT::Container C>
      Request RequestHeap(this C const& self, const size_t count) assumptious {
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

      /// Destroys only the first element.                                    
      ///   @tparam DESTROY set to 'false' if you only want to dereference    
      ///      and destroy only fully dereferenced indirections               
      template<bool DESTROY = true, CT::Container C>
      void DestroyElement(this C& self) assumptious {
         //static_assert(CT::ContainsOne<C>,
         //   "Destroying only first element in a container that may contain many");

         if constexpr (DESTROY) {
            if constexpr (CT::DeeplyOwned<C>) {
               #if LANGULUS_FEATURE(MANAGED_MEMORY)
                  self.DestroyElementDeepCustomPointers();
               #else
                  self.DestroyElementDeepStandardPointers();
               #endif
            }
            else if_available(self.DestroyElementShallow())
            else static_assert(false, "No destruction routine was called");
         }
         else if constexpr (CT::DeeplyOwned<C>) {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               self.template DestroyElementDeepCustomPointers<false>();
            #else
               self.template DestroyElementDeepStandardPointers<false>();
            #endif
            //else static_assert(false, "No destruction routine was called");
         }
      }

      /// Destroys all elements.                                              
      ///   @tparam DESTROY set to 'false' if you only want to dereference    
      ///      and destroy only fully dereferenced indirections               
      template<bool DESTROY = true, CT::Container C>
      void DestroyAllElements(this C& self) assumptious {
         if constexpr (CT::ContainsOne<C>) {
            self.template DestroyElement<DESTROY>();
         }
         else if constexpr (DESTROY or CT::DeeplyOwned<C>) {
            auto item = IterateHandles(self).begin();
            while (item) {
               item->template DestroyElement<DESTROY>();
               ++item;
            }
         }
      }
   };
}
