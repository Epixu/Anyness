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
#include <Langulus/CT/Bool.hpp>
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
   ///   @tparam ENTRIES optional extensions that include more data into      
   ///      the heap allocation. Each ID must correspond to a matching type   
   ///      component ID. Each entry also allows for pointer customization,   
   ///      including support for packed pointers.                            
   ///   @attention only the first ENTRY::T type is used as a heap reference  
   ///      variable on the stack. If no entries are defined, a void* is used.
   template<Cid ID, CT::HeapEntry...ENTRIES>
   struct HeapReference {
      using CTTI_Component = Yes<>;
      using StackRequest   = typename decltype([] {
         if constexpr (sizeof...(ENTRIES) == 0) return Types<void*> {};
         else return Types<typename ENTRIES::T...> {};
      }())::First;

      static constexpr Cid  Id = ID;
      static constexpr Cid  HeapProvider = ID;
      static constexpr int  ComponentPrecedence = -2000;
      static constexpr bool HeapCanBeNull = true;

   protected:
      template<Cid>                 friend struct IterationOperators;
      template<Cid, Cid...>         friend struct Removal;
      template<Cid, Cid...>         friend struct IndexedCommon;
      template<Cid, class, Cid...>  friend struct IndexedCommonHashed;
      template<Cid, Cid...>         friend struct IndexedLinear;
      template<Cid, uint, uint, CT::HeapEntry...> friend struct HeapMovable;
      template<Cid, Cid...>         friend struct Emplacement;
      LglsComComparison(friend);
      template<Cid, Cid...>         friend struct Conversion;
      template<Cid, auto, Cid...>   friend struct CountStatic;
      template<Cid, bool, Cid...>   friend struct OwnershipEmergent;
      template<Cid, bool>           friend struct OwnershipDeepEmergent;
      template<Cid, class, Cid...>  friend struct HashEmergent;
      
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();

   public:
      /// Get a direct access to the heap memory                              
      ///   @attention using raw pointer while self.IsEmpty() may lead to     
      ///      undefined behavior                                             
      template<Cid SID = ID, CT::Container C>
      constexpr auto GetRaw(this C&& self) noexcept {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         using Tcvq = LglsMutIf(C, StackRequest);
         return static_cast<Tcvq>(self.template GetHeapInner<SID>());
      }
      
      /// Get a direct access to the heap memory as a different type          
      ///   @attention using raw pointer while self.IsEmpty() may lead to     
      ///      undefined behavior                                             
      template<class T, Cid SID = ID, CT::Container C>
      constexpr auto GetRawAs(this C&& self) noexcept {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         using Tcvq = LglsMutIf(C, T*);
         return static_cast<Tcvq>(self.template GetHeapInnerAsVoid<SID>());
      }

      /// Get a direct access to the initialized heap memory's end.           
      ///   @attention this makes sense only when heap is contiguous.         
      template<Cid SID = ID, CT::Container C> requires CT::Contiguous<C>
      constexpr auto GetRawEnd(this C&& self) noexcept {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         if constexpr (CT::TypeErased<C>)
            return self.template GetRawAs<uint8_t, SID>() + self.template GetBytesize<SID>();
         else
            return self.template GetRaw<SID>() + self.template GetCount<SID>();
      }
    
      /// Get a direct access to the entire heap reserve's end.               
      template<Cid SID = ID, CT::Container C>
      constexpr auto GetRawReserveEnd(this C&& self) noexcept {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         if constexpr (CT::TypeErased<C>)
            return self.template GetRawAs<uint8_t, SID>() + self.template GetReserved<SID>() * self.template GetStride<SID>();
         else
            return self.template GetRaw<SID>() + self.template GetReserved<SID>();
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
      template<class AS = void, Cid SID = ID, CT::Container C>
      constexpr decltype(auto) Get(this C&& self) assumptious {
         static_assert(not CT::Handle<AS>,    "AS can't be a handle");
         static_assert(not CT::Pair<AS>,      "AS can't be a pair");
         static_assert(not CT::Reference<AS>, "Strip references first");
         using TC   = LglsMutIf(C, TypeOf<C>);
         using TCP  = LglsMutIf(C, TC*);
         using TH   = Tif<CT::Void<AS>, TC, AS>;
         using THP  = LglsMutIf(C, TH*);
         auto& heap = self.template GetHeapInner<SID>();

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

      /// Get first element as a handle, or any desired wrapping type.        
      /// Conversion or copying may occur, depending on type.                 
      ///   @attention will throw if incompatible type is provided            
      ///   @tparam AS the type we're wrapping in                             
      ///   @return the element, as a reference if possible                   
      template<CT::NotVoid AS, CT::Container C> requires CT::Contiguous<C>
      decltype(auto) As(this C&& self) {
         static_assert(not CT::Reference<AS>, "Strip references first");

         if constexpr (CT::Handle<AS>)
            return self.template GetHandle<AS>();
         else {
            // Access directly or wrapped in a container                
            if constexpr (CT::TypeErased<C>) {
               if (self.template Is<AS>()) {
                  // Access directly                                    
                  if constexpr (CT::Deep<AS> and CT::Dense<AS>)
                     return Decvq<AS> {Absorb, self.template Get<AS>()};
                  else
                     return self.template Get<AS>();
               }
               else if constexpr (CT::Deep<AS> and CT::Dense<AS>) {
                  // Wrap in a container                                
                  Decvq<AS> temp {Absorb, self};
                  if_available(temp.SetCountInner(1));
                  return temp;
               }
               else {
                  // Runtime type mismatch error                        
                  LglsError("Type mismatch", ": ", self.GetType(),
                     " not akin to ", MetaDataOf<AS>());

                  if constexpr (CT::Deep<AS> and CT::Dense<AS>)
                     return Decvq<AS> {};
                  else
                     return self.template Get<AS>();
               }
            }
            else {
               if constexpr (Akin<TypeOf<C>, AS>) {
                  // Access directly                                    
                  return self.template Get<AS>();
               }
               else if constexpr (CT::Deep<AS> and CT::Dense<AS>) {
                  // Wrap in a container                                
                  Decvq<AS> temp {Absorb, self};
                  if_available(temp.SetCountInner(1));
                  return temp;
               }
               else static_assert(false, "Type mismatch");
            }
         }
      }

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
      /// Get the heap pointer (inner)                                        
      template<Cid SID = ID>
      constexpr auto& GetHeapInner(this auto&& self) noexcept {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         return self.template AccessStack<HeapReference>();
      }

      /// Get the heap pointer as a void* (inner)                             
      template<Cid SID = ID>
      constexpr void* GetHeapInnerAsVoid(this auto&& self) noexcept {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         auto& p = self.GetHeapInner();
         if constexpr (CT::CustomPointer<StackRequest>)
            return const_cast<void*>(static_cast<void const*>(p.Unpack()));
         else
            return const_cast<void*>(static_cast<void const*>(p));
         //else
         //   return static_cast<void*>(const_cast<DecvqAll<StackRequest>>(p));
      }

      /// Set the heap pointer, any data pointer will do                      
      template<Cid SID = ID, CT::Sparse P>
      /*constexpr*/ void SetHeapInner(this auto& self, P heap) assumptious { //can't be constexpr due to GCC ICE
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         if constexpr (Exact<P, StackRequest>)
            self.GetHeapInner() = heap;
         else if constexpr (CT::CustomPointer<P>)
            self.GetHeapInner() = static_cast<StackRequest>(heap.Unpack());
         else
            self.GetHeapInner() = static_cast<StackRequest>(DecvqAllCast(heap));
      }

      template<Cid SID = ID>
      constexpr void SetHeapInner(this auto& self, nullptr_t) noexcept {
         static_assert(SID == ID or ((SID == ENTRIES::Id) or ...));
         self.GetHeapInner() = nullptr;
      }

      /// Get first element as a handle. Very useful for internal use.        
      /// No-op if C is already a handle, even if AS is specified.            
      ///   @attention element might be uninitialized if C is discontiguous   
      ///   @tparam AS the handle type, or void to decide automatically       
      ///   @tparam SID the shared heap entry ID                              
      ///   @return the handle to the first element. This element might not   
      ///      be initialized if C is discontiguous!                          
      template<class AS = void, Cid SID = ID, CT::NotHandle C>
      decltype(auto) GetHandle(this C&& self) {
         static_assert(CT::Handle<AS> or CT::Void<AS>,
            "Must be either a handle or void (which will use DecideHandle");
         static_assert(not CT::Reference<AS>,
            "Strip references first");
         static_assert(CT::Dense<AS>,
            "Must be dense");

         using H = Tif<CT::Void<AS>, DecideHandle<C>, AS>;
         if constexpr (CT::Pair<H>) {
            // User desires a pair, so we give them a pair              
            using H1 = decltype(H::key);
            using H2 = decltype(H::val);
            return H {
               self.template GetHandle<H1, SID + 0>(),
               self.template GetHandle<H2, SID + 1>()
            };
         }
         else {
            // User desires a simple handle                             
            if constexpr (CT::TypeErased<H>) {
               // Type-erased handle                                    
               if constexpr (CT::DeeplyOwned<H>) {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetEntries<SID>(),
                     self.template GetType<SID>()
                  };
               }
               else if constexpr (CT::Owned<H>) {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetAllocation<SID>(),
                     self.template GetType<SID>()
                  };
               }
               else {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetType<SID>()
                  };
               }
            }
            else {
               // Statically typed handle                               
               using HT = Deref<TypeOf<H>>;

               if constexpr (CT::TypeErased<C>) {
                  LglsAssert(self.template GetType<SID>().IsSame(MetaDataOf<HT>()),
                     "Type mismatch", ": ", self.template GetType<SID>(),
                     " not same as ", MetaDataOf<HT>()
                  );
               }
               else if constexpr (CT::Map<C>) {
                  static_assert(Same<typename TypeOf<C>::template At<SID>, HT>,
                     "Type mismatch"
                  );
               }
               else {
                  static_assert(Same<TypeOf<C>, HT>,
                     "Type mismatch"
                  );
               }

               if constexpr (CT::DeeplyOwned<H>) {
                  return H {
                     &self.template Get<void, SID>(),
                     self.template GetEntries<SID>()
                  };
               }
               else if constexpr (CT::Owned<H>) {
                  return H {
                     &self.template Get<void, SID>(),
                     self.template GetAllocation<SID>()
                  };
               }
               else return H {&self.template Get<void, SID>()};
            }
         }
      }

      /// No-op in case C is already a handle                                 
      template<class AS = void, Cid SID = ID, CT::Handle C>
      constexpr C&& GetHandle(this C&& self) noexcept {
         static_assert(SID == 0);
         return LglsFwd(self);
      }

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
      /// byte size, number of reserved elements, and header/footer offsets.  
      struct Request {
         pot_t  mTotalBytes;
         size_t mHeaderBytes;
         size_t mFooterBytes;
         size_t mReserved;
      };
      
      /// Get a size based on reflected allocation page and count             
      ///   @param reserve the number of elements to request                  
      template<Cid SID = ID, CT::Container C>
      Request RequestHeap(this C const& self, const size_t reserve) assumptious {
         Request result;
         result.mHeaderBytes = self.template GetHeapHeaderSize<SID>();

         if constexpr (C::CountHeapFooterRequests()) {
            // When there are footer requests (heap requests that          
            // depend on count & indirections), we aren't allowed to       
            // change the requested reserve to avoid heap corruptions.     
            result.mFooterBytes = self.GetHeapFooterSize(reserve);

            if constexpr (CT::TypeErased<C>) {
               // Check for reflected minimal allocation at runtime        
               const auto T = self.GetType();
               LglsAssumeDev(T, "Requesting allocation size for an untyped container");
               const auto size = T.GetSize();
               result.mTotalBytes = Roof2(//::std::max(
                  reserve * size + result.mHeaderBytes + result.mFooterBytes/*,
                  static_cast<size_t>(T.GetMinAllocation())
               )*/);
            }
            else {
               // Check for reflected minimal allocation at compile-time   
               using T = TypeOf<C>;
               result.mTotalBytes = Roof2(//::std::max(
                  reserve * sizeof(T) + result.mHeaderBytes + result.mFooterBytes/*,
                  CT::GetMinAlloc<T>()
               )*/);
            }

            result.mReserved = reserve;
         }
         else {
            // When there are no footer requests, we are allowed to        
            // reserve more bytes than requested.                          
            result.mFooterBytes = 0;

            if constexpr (CT::TypeErased<C>) {
               // Check for reflected minimal allocation at runtime        
               const auto T = self.GetType();
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
               using T = TypeOf<C>;
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
      ///   @tparam DESTROY set to 'false' if you only want to dereference    
      ///      and destroy only fully dereferenced indirections               
      template<bool DESTROY = true, CT::Container C>
      void DestroyElement(this C& self) assumptious {
         static_assert(CT::ContainsOne<C>,
            "Destroying only first element in a container with many. GetHandle() first?");

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
         }
      }

      /// Destroys all elements.                                              
      ///   @tparam DESTROY set to 'false' if you only want to dereference    
      ///      and destroy only fully dereferenced indirections               
      template<bool DESTROY = true, CT::Container C>
      void DestroyAllElements(this C& self) assumptious {
         if constexpr (DESTROY or CT::DeeplyOwned<C>) {
            if (self.IsEmpty())
               return;

            self.Apply([](auto&& item) {
               if constexpr (CT::Supported<decltype(item)>)
                  item.template DestroyElement<DESTROY>();
            });
         }
      }

      /// Visit all element's handles and perform a function on them.         
      /// Handles both linear and non-linear containers gracefully.           
      ///   @param lambda the function to perform. If the lambda returns bool,
      ///      you can end the loop early by returning false.                 
      ///   @param cookie the element/hash table spot to start off from       
      template<CT::Container C>
      void Apply(this C&& self, auto&& lambda, [[maybe_unused]] size_t cookie = 0) {
         LglsAssumeDev(not self.IsEmpty(), "Make sure container isn't empty");

         if constexpr (CT::ContainsOne<C>) {
            //TODO GetHandle here is redundant, but most use cases      
            // of Apply require it.                                     
            lambda(self.GetHandle());
         }
         else {
            auto item = self.GetHandle() + cookie;

            if constexpr (CT::Contiguous<C>) {
               // Iterate a contiguous array of elements                
               LglsAssumeDev(cookie < self.GetCount(), "Limp cookie (contiguous)");
               auto const end = item + (self.GetCount() - cookie);
               while (item.GetRaw() != end.GetRaw()) {
                  if constexpr (CT::Bool<decltype(lambda(item))>) {
                     if (not lambda(item))
                        return;
                  }
                  else lambda(item);
                  ++item;
               }
            }
            else {
               // Iterate a hash table - some cells might be empty,     
               // thus container might not be a contiguous array        
               LglsAssumeDev(cookie < self.GetReserved(), "Limp cookie (discontiguous)");
               const auto tableBeg = self.GetHashTableInner() + cookie;
               const auto tableEnd = tableBeg + (self.GetReserved() - cookie);
               auto table = tableBeg;
               while (table != tableEnd) {
                  if (*table) {
                     if constexpr (CT::Bool<decltype(lambda(item))>) {
                        if (not lambda(item))
                           return;
                     }
                     else lambda(item);
                  }
                  else {
                     if constexpr (CT::Bool<decltype(lambda(Unsupported{}))>) {
                        if (not lambda(Unsupported{}))
                           return;
                     }
                     else lambda(Unsupported{});
                  }

                  ++item;
                  ++table;
               }
            }
         }
      }
   };
}
