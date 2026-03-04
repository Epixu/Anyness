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
   /// Adds a variable to a container                                         
   /// Increases the container's bytesize                                     
   ///   @tparam T type of the variable                                       
   ///   @tparam ID multiple variables are supported                          
   template<CT::NotVoid T, Cid ID>
   struct Stack {
      using CTTI_Component = Yes<>;
      using StackRequest   = T;
      
      static constexpr Cid Id = ID;
      static constexpr Cid StackProvider = ID;
      static constexpr int ComponentPrecedence = -2000;
      
   protected:
      /// Get the heap pointer (inner)                                        
      constexpr auto& GetStackInner(this auto&& self) noexcept {
         return self.template AccessStack<Stack>();
      }

      constexpr void SetStackInner(this auto& self, T&& data) noexcept {
         self.GetStackInner() = LglsFwd(data);
      }

   public:
      /// Get a direct access to the stack memory                             
      template<CT::Container C>
      constexpr auto GetRaw(this C&& self) noexcept {
         return &self.GetStackInner();
      }

      /// Get a direct access to the stack memory as a different type         
      template<class AS, CT::Container C>
      constexpr auto GetRawAs(this C&& self) noexcept {
         using AScvq = LglsMutIf(C, AS*);
         return static_cast<AScvq>(self.GetRaw());
      }

      /// Get a direct access to the stack memory's end                       
      template<CT::Container C>
      constexpr auto GetRawEnd(this C&& self) noexcept {
         return self.GetRaw() + 1;
      }

      /// Get a direct access to the stack memory's end                       
      template<CT::Container C>
      constexpr auto GetRawReserveEnd(this C&& self) noexcept {
         return self.GetRawEnd();
      }

      /// Get reference to first element as sparse or dense, depending on T.  
      /// This is a lower-level routine that does only sparseness checking.   
      /// No conversion or copying occurs, only pointer arithmetic.           
      ///   @attention no type-safety                                         
      ///   @tparam AS the type of data we're accessing - use void to use the 
      ///      type of the stack                                              
      template<class AS = void, CT::Container C>
      constexpr decltype(auto) Get(this C&& self) assumptious {
         static_assert(not CT::Handle<AS>,    "AS can't be a handle");
         static_assert(not CT::Reference<AS>, "Strip references first");
         using TC   = LglsMutIf(C, T);
         using TCP  = LglsMutIf(C, TC*);
         using TH   = Tif<CT::Void<AS>, TC, AS>;
         using THP  = LglsMutIf(C, TH*);
         auto& stack = self.GetStackInner();

         // Casting to a desired static type                            
         if constexpr (IndirectsOf<TC> == IndirectsOf<TH>) {
            // No difference in indirections                            
            return *static_cast<THP>(static_cast<TCP>(&stack));
         }
         else if constexpr (IndirectsOf<TC> > IndirectsOf<TH>) {
            // We need to dereference. Can be done without a            
            // reinterpret_cast, and thus be constexpr-friendly.        
            // Supports packed pointers as well.                        
            return *static_cast<THP>(DenseCast<IndirectsOf<TC> - IndirectsOf<TH>>(static_cast<TCP>(&stack)));
         }
         else {
            // We are allowed to add one additional indirection         
            static_assert(IndirectsOf<TCP> == IndirectsOf<TH>,
               "Too many indirections");
            return *const_cast<THP>(reinterpret_cast<ConstAll<THP>>(&stack));
         }
      }

      /// Get first element as a handle, or any desired wrapping type.        
      /// Conversion or copying may occur, depending on type.                 
      ///   @tparam AS the type we're wrapping in                             
      ///   @return the element, as a reference if possible                   
      template<CT::NotVoid AS, CT::Container C> requires CT::Contiguous<C>
      decltype(auto) As(this C&& self) {
         static_assert(not CT::Reference<AS>, "Strip references first");

         if constexpr (CT::Handle<AS>)
            return self.template GetHandle<AS>();
         else {
            // Access directly or wrapped in a container                
            if constexpr (Akin<T, AS>) {
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

         if constexpr (CT::Resolvable<T>)
            return D {DenseCast(self.template Get<T>()).GetResolved()};
         else
            return D {DenseCast(self.template Get<T>())};
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
      auto GetDense(this C&& self, size_t count = -1)
      requires requires { typename Deref<C>::DeepType; } {
         using D = Tif<CT::Void<AS>, typename Deref<C>::DeepType, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         LglsAssert(not self.IsEmpty(), "Can't GetDense from empty container");
         if (not self.IsSparse() or count <= 0)
            return D {Absorb, Disown(self)};

         // Check if origin type is complete before attempting anything 
         if (count >= IndirectsOf<T>) {
            LglsAssert(CT::Complete<Decay<T>>,
               "Trying to interface incomplete data `", self.GetType(),
               "` as dense"
            );
         }

         void* src = DecvqAllCast(self.GetHeapInner());
         auto type = self.GetType();
         while (count and type.IsSparse()) {
            auto nextType = type.GetDeptr();
            
            if (nextType.IsSparse()) {
               // Pointer T -> Pointer nextT                            
               type.GetDereffer()(src, &src);
            }
            else {
               // Pointer T -> Dense nextT                              
               D temp {Absorb, Disown(self)};
               temp.SetTypeInner(nextType);
               temp.SetHeapInner(UnpackPointer(type, nextType, src));
               if_available(temp.SetCountInner(1));
               return temp;
            }

            type = nextType;
            --count;
         }
         
         LglsError("Should never be reached");
         return D {Absorb, Disown(self)};
      }

   protected:      
      /// Get first element as a handle. Very useful for internal use.        
      ///   @attention element might be uninitialized if C is discontiguous   
      ///   @tparam AS the handle type, or void to decide automatically       
      ///   @return the handle to the first element                           
      template<class AS = void, CT::Container C>
      decltype(auto) GetHandle(this C&& self) {
         static_assert(CT::Handle<AS> or CT::Void<AS>,
            "Must be either a handle or void (which will use DecideHandle");
         static_assert(not CT::Reference<AS>, "Strip references first");
         static_assert(CT::Dense<AS>, "Must be dense");

         using H = Tif<CT::Void<AS>, DecideHandle<C>, AS>;

         if constexpr (CT::TypeErased<H>) {
            // Type-erased handle                                       
            if constexpr (CT::DeeplyOwned<H>)
               return H {self.Get(), self.GetEntries(), self.GetType()};
            else if constexpr (CT::Owned<H>)
               return H {self.Get(), self.GetAllocation(), self.GetType()};
            else
               return H {self.Get(), self.GetType()};
         }
         else {
            // Statically typed handle                                  
            using HT = Deref<TypeOf<H>>;

            if constexpr (CT::TypeErased<C>) {
               LglsAssert(self.template IsSame<HT>(), "Type mismatch",
                  ": ", self.GetType(), " not same as ", MetaDataOf<HT>());
            }
            else static_assert(Same<TypeOf<C>, HT>, "Type mismatch");

            if constexpr (CT::DeeplyOwned<H>)
               return H {&self.Get(), self.GetEntries()};
            else if constexpr (CT::Owned<H>)
               return H {&self.Get(), self.GetAllocation()};
            else
               return H {&self.Get()};
         }
      }

      /// Default-initialize count to zero                                    
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetStackInner({});
      }
   };
}
