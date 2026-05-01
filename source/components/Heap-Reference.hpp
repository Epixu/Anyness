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
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.HeapReference<ENTRY0, ENTRYN...>

   ///                                                                        
   /// Adds a variable to a container that only references a remote heap.     
   /// No allocation interface is provided.                                   
   /// Increases the container's bytesize.                                    
   ///   @tparam ENTRY0 first heap provider                                   
   ///   @tparam ENTRYN optional extensions that include more data into       
   ///      the heap allocation. Each ID must correspond to a matching type   
   ///      component ID. Each entry also allows for pointer customization,   
   ///      including support for packed pointers.                            
   ///   @attention only the first ENTRY0::T type is used as a heap reference 
   ///      variable on the stack.                                            
   template<CT::HeapEntry ENTRY0, CT::HeapEntry...ENTRYN>
   struct HeapReference {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using StackRequest   = typename ENTRY0::T;

      static constexpr Cid  Id = ENTRY0::Id;
      static constexpr Cid  HeapProvider = ENTRY0::Id;
      static constexpr int  ComponentPrecedence = -2000;
      static constexpr bool HeapCanBeNull = true;
      template<Cid SID>
      static constexpr bool Relevant = IdMatch<SID, ENTRY0::Id, ENTRYN::Id...>;

   protected:
      LglsComIterationOperators(friend);
      LglsComRemoval(friend);
      LglsComIndexedCommon(friend);
      LglsComIndexedCommonHashed(friend);
      LglsComIndexedLinear(friend);
      LglsComHeapMovable(friend);
      LglsComEmplacement(friend);
      LglsComComparison(friend);
      LglsComConversion(friend);
      LglsComCountStatic(friend);
      LglsComOwnershipEmergent(friend);
      LglsComOwnershipDeepEmergent(friend);
      LglsComHashEmergent(friend);
      
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();

   public:
      /// Get a direct access to the heap memory                              
      ///   @attention using raw pointer while self.IsEmpty() may lead to     
      ///      undefined behavior                                             
      template<Cid SID = Id, CT::Container C> requires Relevant<SID>
      constexpr auto GetRaw(this C&& self) noexcept {
         using Tcvq = LglsMutIf(C, StackRequest);
         return static_cast<Tcvq>(ThisCom::GetHeapInner());
         //TODO offset pointer based on dimension
      }
      
      /// Get a direct access to the heap memory as a different type          
      ///   @attention using raw pointer while self.IsEmpty() may lead to     
      ///      undefined behavior                                             
      template<class T, Cid SID = Id, CT::Container C> requires Relevant<SID>
      constexpr auto GetRawAs(this C&& self) noexcept {
         using Tcvq = LglsMutIf(C, T*);
         return static_cast<Tcvq>(ThisCom::GetRawVoid());
      }

      /// Get a direct access to the initialized heap memory's end.           
      ///   @attention this makes sense only when heap is contiguous.         
      template<Cid SID = Id, CT::Container C> requires (CT::Contiguous<C> and Relevant<SID>)
      constexpr auto GetRawEnd(this C&& self) noexcept {
         if constexpr (CT::TypeErased<C>)
            return ThisCom::template GetRawAs<uint8_t, SID>() + self.template GetBytesize<SID>();
         else
            return ThisCom::template GetRaw<SID>() + self.template GetCount<SID>();
      }
    
      /// Get a direct access to the entire heap reserve's end.               
      template<Cid SID = Id, CT::Container C> requires Relevant<SID>
      constexpr auto GetRawReserveEnd(this C&& self) noexcept {
         const auto reserved = self.template GetReserved<SID>();
         if constexpr (CT::TypeErased<C>)
            return ThisCom::template GetRawAs<uint8_t, SID>() + reserved * self.template GetStride<SID>();
         else
            return ThisCom::template GetRaw<SID>() + reserved;
      }
    
      /// Get reference to first element as sparse or dense, depending on T.  
      /// This is a lower-level routine that does only sparseness checking.   
      /// No conversion or copying occurs, only pointer arithmetic.           
      ///   @attention element might be uninitialized if C is discontiguous   
      ///   @attention no type-safety                                         
      ///   @attention assumes the container is typed                         
      ///   @attention assumes the container has valid heap                   
      ///   @tparam AS the type of data we're accessing - use void to use the 
      ///      type of the container, if statically typed                     
      template<class AS = void, Cid SID = Id, CT::Container C> requires Relevant<SID>
      constexpr decltype(auto) Get(this C&& self) assumptious {
         static_assert(not CT::Handle<AS>,    "AS can't be a handle");
         static_assert(not CT::Reference<AS>, "Strip references first");

         using TC   = LglsMutIf(C, TypeOf<C>);
         using TCP  = LglsMutIf(C, TC*);
         using TH   = Tif<CT::Void<AS>, TC, AS>;
         using THP  = LglsMutIf(C, TH*);
         auto& heap = ThisCom::GetHeapInner();

         if constexpr (CT::TypeErased<C>) {
            if constexpr (CT::Void<AS>) {
               // Unknown type, just return the heap pointer reference  
               return (heap);
            }
            else {
               // Casting to a desired runtime type                     
               LglsAssumeDev(self.template IsTyped<SID>(), "Block is not typed");
               const auto indirections = self.template GetIndirections<SID>();

               if (indirections == IndirectsOf<TH>) {
                  // No difference in indirections                      
                  return *static_cast<THP>(heap);
               }
               else if (indirections > IndirectsOf<TH>) {
                  // We need to dereference. Supports packed pointers.  
                  auto diff = indirections - IndirectsOf<TH>;
                  using Deep = typename Deref<C>::DeepType;
                  Deep denser = Disown(ThisCom::template GetDense<SID>(diff));
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
               return *const_cast<THP>(static_cast<ConstAll<THP>>(static_cast<ConstAll<TCP>>(heap)));
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

      /// Get first element as a handle, or any desired wrapping type.        
      /// Conversion or copying may occur, depending on type.                 
      ///   @attention will throw if incompatible type is provided            
      ///   @tparam AS the type we're wrapping in                             
      ///   @return the element, as a reference if possible                   
      template<CT::NotVoid AS, Cid SID = Id, CT::Container C>
      requires (CT::Contiguous<C> and Relevant<SID>)
      decltype(auto) As(this C&& self) {
         static_assert(not CT::Reference<AS>, "Strip references first");

         if constexpr (CT::Handle<AS>)
            return self.template GetHandle<AS>();
         else {
            // Access directly or wrapped in a container                
            if constexpr (CT::TypeErased<C>) {
               if (self.template Is<AS, SID>()) {
                  // Access directly                                    
                  if constexpr (CT::DeepDense<AS>)
                     return Decvq<AS> {Absorb, ThisCom::template Get<AS, SID>()};
                  else
                     return ThisCom::template Get<AS, SID>();
               }
               else if constexpr (CT::DeepDense<AS>) {
                  // Wrap in a container                                
                  Decvq<AS> temp {Absorb, self};
                  if_available(temp.template SetCountInner<SID>(1));
                  return temp;
               }
               else {
                  // Runtime type mismatch error                        
                  LglsError("Type mismatch", ": ", self.template GetType<SID>(),
                     " not akin to ", MetaDataOf<AS>());

                  if constexpr (CT::DeepDense<AS>)
                     return Decvq<AS> {};
                  else
                     return ThisCom::template Get<AS, SID>();
               }
            }
            else {
               if constexpr (Akin<TypeOf<C, SID>, AS>) {
                  // Access directly                                    
                  return ThisCom::template Get<AS, SID>();
               }
               else if constexpr (CT::DeepDense<AS>) {
                  // Wrap in a container                                
                  Decvq<AS> temp {Absorb, self};
                  if_available(temp.template SetCountInner<SID>(1));
                  return temp;
               }
               else static_assert(false, "Type mismatch");
            }
         }
      }

      /// A safe way to get the first sparse entry after being resolved to    
      /// the most concrete type. Available only if container has DeepType.   
      ///   @return the most concrete representation of the first item        
      template<Cid SID = Id, class AS = void, CT::Container C>
      requires (CT::Contiguous<C> and Relevant<SID>)
      auto GetResolved(this C&& self)
      requires requires { typename Deref<C>::DeepType; } {
         using D = Tif<CT::Void<AS>, typename Deref<C>::DeepType, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         static_assert(CT::HasVariableCount<D>, "D must allow for being empty");

         if (self.template IsEmpty<SID>())
            return D {};
         if (not self.template IsSparse<SID>())
            return ThisCom::template As<D, SID>();

         if constexpr (CT::TypeErased<C>) {
            const auto T = self.template GetType<SID>();
            const auto resolver = T.GetResolver();
            if (resolver)
               return D {resolver(ThisCom::template GetDense<SID>().GetRaw())};
            else
               return ThisCom::template GetDense<SID, D>();
         }
         else {
            using T = TypeOf<C, SID>;
            if constexpr (CT::Resolvable<T>)
               return D {DenseCast(ThisCom::template Get<T, SID>()).GetResolved()};
            else
               return D {DenseCast(ThisCom::template Get<T, SID>())};
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
      template<Cid SID = Id, class AS = void, CT::Container C>
      requires (CT::Contiguous<C> and Relevant<SID>)
      auto GetDense(this C&& self, size_t count = -1 /*Count<C> count = CountMax<C>*/)
      requires requires { typename Deref<C>::DeepType; } {
         using D = Tif<CT::Void<AS>, typename Deref<C>::DeepType, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");

         if (self.template IsEmpty<SID>())
            return D {};
         if (count <= 0 or not self.template IsSparse<SID>())
            return D {Absorb, Disown(self)};

         // Check if origin type is complete before attempting anything 
         if constexpr (CT::TypeErased<C>) {
            const auto T = self.template GetType<SID>();
            if (count >= T.GetIndirections()) {
               LglsAssert(T.GetOrigin(),
                  "Trying to interface incomplete data `", self.template GetType<SID>(),
                  "` as dense"
               );
            }
         }
         else {
            using T = TypeOf<C, SID>;
            if (count >= IndirectsOf<T>) {
               LglsAssert(CT::Complete<Decay<T>>,
                  "Trying to interface incomplete data `", self.template GetType<SID>(),
                  "` as dense"
               );
            }
         }

         void* src = DecvqAllCast(ThisCom::GetHeapInner());
         auto T = self.template GetType<SID>();
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
      //template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Get the heap pointer (inner)                                        
      template<Cid SID = Id> requires Relevant<SID>
      constexpr auto& GetHeapInner(this auto&& self) noexcept {
         return self.template AccessStack<HeapReference>();
      }

      /// Get the heap pointer as a void* (inner)                             
      /*template<Cid SID = ID> requires IdMatch<SID, ID, ENTRIES::Id...>
      constexpr void* GetHeapInnerAsVoid(this auto&& self) noexcept {
         auto& p = ThisCom::GetHeapInner();
         if constexpr (CT::CustomPointer<StackRequest>)
            return const_cast<void*>(static_cast<void const*>(p.Unpack()));
         else
            return const_cast<void*>(static_cast<void const*>(p));
      }*/

      /// Get a direct access to the heap memory                              
      ///   @attention using raw pointer while self.IsEmpty() may lead to     
      ///      undefined behavior                                             
      template<Cid SID = Id, CT::Container C> requires Relevant<SID>
      constexpr void* GetRawVoid(this C&& self) noexcept {
         return const_cast<void*>(static_cast<const void*>(self.template GetRaw<SID>()));
      }

      /// Set the heap pointer, any data pointer will do                      
      template<Cid SID = Id, CT::Sparse P> requires Relevant<SID>
      constexpr void SetHeapInner(this auto& self, P heap) assumptious {
         if constexpr (Exact<P, StackRequest>)
            ThisCom::GetHeapInner() = heap;
         else if constexpr (CT::CustomPointer<P>)
            ThisCom::GetHeapInner() = static_cast<StackRequest>(heap.Unpack());
         else
            ThisCom::GetHeapInner() = const_cast<StackRequest>(static_cast<DecvqAll<StackRequest>>(DecvqAllCast(heap)));
      }

      /// Reset the heap pointer to null                                      
      template<Cid SID = Id> requires Relevant<SID>
      constexpr void SetHeapInner(this auto& self, nullptr_t) noexcept {
         ThisCom::GetHeapInner() = nullptr;
      }

      /// Default-initialization of this component                            
      void ConstructDefault(this auto& self) noexcept {
         ThisCom::SetHeapInner(nullptr);
      }
      
      /// Transfer from any kind of container.                                
      /// This is only a reference to a heap allocation and is not allowed    
      /// to allocate any new memory, so all this does is copy the heap       
      /// pointer, ignoring any intents.                                      
      ///   @param self deduced this                                          
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) noexcept {
         ThisCom::SetHeapInner(intent.what.GetHeapInner());
      }

      /// A simple request for allocating memory, which includes heap         
      /// byte size, number of reserved elements, and header/footer offsets.  
      struct Request {
         pot_t  mTotalBytes;
         size_t mHeaderBytes;
         size_t mFooterBytes;
         size_t mReserved;
      };
      
      /// Get a size based on reflected allocation page and count             
      ///   @param reserve the number of elements to request                  
      template<Cid SID = Id, CT::Container C> requires Relevant<SID>
      Request RequestHeap(this C const& self, const size_t reserve) assumptious {
         Request result;
         result.mHeaderBytes = self.template GetHeapHeaderSize<SID>();

         if constexpr (C::CountHeapFooterRequests()) {
            // When there are footer requests (heap requests that       
            // depend on count & indirections), we aren't allowed to    
            // change the requested reserve to avoid heap corruptions.  
            result.mFooterBytes = self.template GetHeapFooterSize<SID>(reserve);

            if constexpr (CT::TypeErased<C>) {
               // Check for reflected minimal allocation at runtime     
               const auto T = self.template GetType<SID>();
               LglsAssumeDev(T, "Requesting allocation size for an untyped container");
               const auto size = T.GetSize();
               result.mTotalBytes = Roof2(
                  reserve * size + result.mHeaderBytes + result.mFooterBytes
               );
            }
            else {
               // Check for reflected minimal allocation at compile-time
               using T = TypeOf<C, SID>;
               result.mTotalBytes = Roof2(
                  reserve * sizeof(T) + result.mHeaderBytes + result.mFooterBytes
               );
            }

            result.mReserved = reserve;
         }
         else {
            // When there are no footer requests, we are allowed to     
            // reserve more bytes than requested.                       
            result.mFooterBytes = 0;

            if constexpr (CT::TypeErased<C>) {
               // Check for reflected minimal allocation at runtime     
               const auto T = self.template GetType<SID>();
               LglsAssumeDev(T, "Requesting allocation size for an untyped container");
               const auto size = T.GetSize();
               result.mTotalBytes = Roof2(::std::max(
                  reserve * size + result.mHeaderBytes,
                  static_cast<size_t>(T.GetMinAllocation())
               ));
               result.mReserved = (result.mTotalBytes - result.mHeaderBytes) / size;
            }
            else {
               // Check for reflected minimal allocation at compile-time
               using T = TypeOf<C, SID>;
               result.mTotalBytes = Roof2(::std::max(
                  reserve * sizeof(T) + result.mHeaderBytes,
                  CT::GetMinAlloc<T>()
               ));
               result.mReserved = (result.mTotalBytes - result.mHeaderBytes) / sizeof(T);
            }
         }

         LglsAssumeDev(result.mReserved >= reserve);
         return result;
      }

      /// Destroys only the first element.                                    
      ///   @tparam FORCE_DESTROY set to 'false' to only dereference.         
      ///      It will still destroy the element, but only when fully         
      ///      dereferenced in all its indirections.                          
      template<bool FORCE_DESTROY = true, Cid SID = Id, CT::Container C> requires Relevant<SID>
      void DestroyElement(this C& self) assumptious {
         static_assert(CT::ContainsOne<C>,
            "Destroying only first element in a container with many. GetHandle() first?");

         if constexpr (FORCE_DESTROY) {
            if constexpr (CT::DeeplyOwned<C>)
               self.template DestroyElementDeep<true, SID>();
            else if constexpr (CT::Owned<C>)
               self.template DestroyElementShallow<SID>();
            else static_assert(false, "No destruction routine was called");
         }
         else if constexpr (CT::DeeplyOwned<C>)
            self.template DestroyElementDeep<false, SID>();
      }

      /// Destroys all elements.                                              
      ///   @tparam FORCE_DESTROY set to 'false' to only dereference.         
      ///      It will still destroy the element, but only when fully         
      ///      dereferenced in all its indirections.                          
      template<bool FORCE_DESTROY = true, Cid SID = Id, CT::Container C> requires Relevant<SID>
      void DestroyAllElements(this C& self) assumptious {
         if constexpr (FORCE_DESTROY or CT::DeeplyOwned<C>) {
            if (self.template IsEmpty<SID>())
               return;

            self.Apply([](auto&& item) {
               item.template DestroyElement<FORCE_DESTROY>();
            });
         }
      }
   };

   #undef ThisCom
}
