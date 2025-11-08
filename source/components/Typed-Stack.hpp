///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/MetaOf.hpp>
#include <Langulus/CT/Akin.hpp>
#include <Langulus/CT/Deep.hpp>


namespace Langulus::Anyness
{
   using DMeta = RTTI::DMeta;
   using TMeta = RTTI::TMeta;
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Defines the contained type as a member variable, allowing the use of   
   /// type-erasure. You can optionally constrain the type.                   
   ///   @tparam META - the type of the meta                                  
   ///   @tparam TYPE - optionally static type, use void for type-erasure     
   ///   @tparam CONSTRAIN - override type-constraint                         
   ///   @tparam ID   - which heap/stack is typed?                            
   template<class META, class TYPE, bool CONSTRAIN, unsigned ID>
   struct TypedStack {
      using CTTI_Component = Yes<>;
      using CTTI_Typed     = TYPE;
      using StackRequest   = META;

      static constexpr int  ComponentPrecedence = -3000;
      static constexpr bool TypeErased = CT::Void<TYPE>;
      /// @attention valid only if not TypeErased                             
      static constexpr bool Sparse = not TypeErased and CT::Sparse<TYPE>;
      /// @attention valid only if not TypeErased                             
      static constexpr bool Dense = not TypeErased and CT::Dense<TYPE>;

   protected:
      template<unsigned> friend struct Removal;
      template<unsigned> friend struct HeapMovable;
      template<unsigned> friend struct Emplacement;

      /// Reset the type of the container, unless it's type-constrained.      
      /// If this container isn't type-erased, this call is a no-op.          
      constexpr void ResetType(this auto& self) noexcept {
         if constexpr (TypeErased) {
            if constexpr (requires { self.IsTypeConstrained(); }) {
               if (not self.IsTypeConstrained())
                  self.SetTypeInner({});
            }
            else self.SetTypeInner({});
         }
      }
      
      /// Get the contained type (inner)                                      
      constexpr auto& GetTypeInner(this auto&& self) noexcept {
         return self.template AccessStack<TypedStack>();
      }

      /// Set the contained type (inner)                                      
      constexpr void SetTypeInner(this auto& self, const META& type) noexcept {
         self.GetTypeInner() = type;
      }

   public:
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this is noop when constructing from deep intents,      
      ///      since element constructors might throw and stuff be partially  
      ///      inserted. In those cases, count is set by the heap components. 
      ///   @param intent - the intent and container to transfer from         
      template<CT::Intent I, CT::Container C> requires CT::Container<I>
      void ConstructFrom(this C& self, I&& intent) {
         if constexpr (I::IsShallow() and not CT::Copied<I>) {
            if constexpr (CT::TypeErased<C>) {
               self.SetType(intent->GetType());

               // While we are interfacing external memory, we have to  
               // keep the type-constrained state, otherwise we risk    
               // interpreting contents the wrong way                   
               if constexpr (not CT::TypeErased<I>)
                  self.EnableTypeConstrained();
               else if (intent->IsTypeConstrained())
                  self.EnableTypeConstrained();
            }
            else {
               if constexpr (not CT::TypeErased<I>)
                  self.template SetType<TypeOf<Deint<I>>>();
               else
                  self.SetType(intent->GetType());
            }
         }
      }
      
      /// Get the contained type - not possible at compile-time yet           
      constexpr META GetType(this auto const& self) noexcept {
         if consteval {
            return META {};
         }
         else {
            META const& meta = self.GetTypeInner();
            if constexpr (not TypeErased)
               const_cast<META&>(meta) = MetaDataOf<TYPE>();
            return meta;
         }
      }

      /// Get the size of a single element in bytes                           
      constexpr size_t GetStride(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().GetSize();
         else
            return sizeof(TYPE);
      }

      /// Get the alignment of a single element in bytes                      
      constexpr pot_t GetAlignment(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().GetAlignment();
         else
            return pot_t(alignof(TYPE));
      }

      /// Get the reflected type name                                         
      constexpr auto GetName(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().GetName();
         else
            return NameOf<TYPE>();
      }

      /// Check if block has a data type                                      
      ///   @return true if data contained in this pack is specified          
      constexpr bool IsTyped(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return static_cast<bool>(self.GetTypeInner());
         else
            return true;
      }

      /// Check if type origin is the same as one of the provided types       
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @tparam A1, AN... - the types to compare against                  
      ///   @return true if origin type is same to at least one of the types  
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool Is(this auto const& self) noexcept {
         if constexpr (TypeErased) {
            const auto& t = self.GetTypeInner();
            return t.Is(MetaDataOf<A1>()) or (t.Is(MetaDataOf<AN>()) or ...);
         }
         else return AkinAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if type origin is the same as another                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param type - the type to check for                               
      ///   @return true if this container has similar data                   
      bool Is(this auto const& self, META type) noexcept {
         return self.GetTypeInner().Is(type);
      }

      /// Check if type origin is the same as another container's type        
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param other - the type to check for                              
      ///   @return true if this container has similar data                   
      template<CT::Container C>
      constexpr bool Is(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return self.GetTypeInner().Is(other.mType);
         else
            return Akin<TYPE, TypeOf<C>>;
      }

      /// Check if unqualified type is the same as one of the provided types  
      ///   @attention ignores only cv-qualifiers                             
      ///   @tparam A1, AN... - the types to compare against                  
      ///   @return true if data type is similar to at least one of the types 
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool IsSame(this auto const& self) noexcept {
         if constexpr (TypeErased) {
            const auto& t = self.GetTypeInner();
            return t.IsSame(MetaDataOf<A1>()) or (t.IsSame(MetaDataOf<AN>()) or ...);
         }
         else return SameAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if unqualified type is the same as another                    
      ///   @attention ignores only cv-qualifiers                             
      ///   @param type - the type to check for                               
      ///   @return true if this block contains similar data                  
      bool IsSame(this auto const& self, META type) noexcept {
         return self.GetTypeInner().IsSame(type);
      }

      /// Check if unqualified type is the same as another container's type   
      ///   @attention ignores only cv-qualifiers                             
      ///   @param other - the container to check for                         
      ///   @return true if this container has similar data                   
      template<CT::Container C>
      constexpr bool IsSame(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return self.GetTypeInner().IsSame(other.mType);
         else
            return Same<TYPE, TypeOf<C>>;
      }

      /// Check if this type is exactly one of the provided types             
      ///   @tparam T1, TN... - the types to compare against                  
      ///   @return true if data type matches at least one type               
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool IsExact(this auto const& self) noexcept {
         if constexpr (TypeErased) {
            const auto& t = self.GetTypeInner();
            return t.IsExact(MetaDataOf<A1>()) or (t.IsExact(MetaDataOf<AN>()) or ...);
         }
         else return ExactAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if this type is exactly another                               
      ///   @param type - the type to match                                   
      ///   @return true if data type matches type exactly                    
      bool IsExact(this auto const& self, META type) noexcept {
         return self.GetTypeInner().IsExact(type);
      }

      /// Check if this type is exactly another container's type              
      ///   @param other - the block to match                                 
      ///   @return true if data type matches type exactly                    
      template<CT::Container C>
      constexpr bool IsExact(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return self.GetTypeInner().IsExact(other.mType);
         else
            return Exact<TYPE, TypeOf<C>>;
      }
      
      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      constexpr bool IsSparse(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().IsSparse();
         else
            return CT::Sparse<TYPE>;
      }
      
      /// Get the number of indirections                                      
      /// int**** will result in 4; int* will result in 1, int results in 0.  
      constexpr size_t GetIndirections(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().GetIndirections();
         else
            return IndirectsOf<TYPE>;
      }
      
      /// Check if block is constant                                          
      ///   @attention disowned containers are always constant                
      ///   @return true if the contents are constant                         
      constexpr bool IsConstant(this auto const& self) noexcept {
         if constexpr (requires { self.GetAllocation(); }) {
            if constexpr (TypeErased)
               return not self.GetAllocation() or self.GetTypeInner().IsConstant();
            else
               return CT::Constant<TYPE> or not self.GetAllocation();
         }
         else {
            if constexpr (TypeErased)
               return self.GetTypeInner().IsConstant();
            else
               return CT::Constant<TYPE>;
         }
      }

      /// Check if container is made of other containers                      
      ///   @return true if the container is deep                             
      constexpr bool IsDeep(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().IsDeep();
         else
            return CT::Deep<TYPE>;
      }

      /// Returns true if a type constraint is specified                      
      constexpr bool IsTypeConstrained() const noexcept {
         return CONSTRAIN;
      }

      constexpr void EnableTypeConstrained() const noexcept {
         static_assert(CONSTRAIN,
            "Can't enable type-constraint in type-erased container. "
            "Make sure you've added Typed state and properly disambiguated it"
         );
      }

      constexpr void DisableTypeConstrained() const noexcept {
         static_assert(not CONSTRAIN,
            "Can't disable type-constraint in a statically-typed container. "
            "Make sure you've added Typed state and properly disambiguated it"
         );
      }

      /// Get the size of the type times the contained elements               
      ///   @return the size of all elements in bytes                         
      constexpr size_t GetBytesize(this auto const& self) noexcept {
         return self.GetStride() * self.GetCount();
      }

      template<bool BINARY_COMPATIBLE = false, bool ADVANCED = false>
      bool CastsToMeta(META) const;
      template<bool BINARY_COMPATIBLE = false>
      bool CastsToMeta(META, size_t) const;

      template<CT::NotVoid, bool BINARY_COMPATIBLE = false, bool ADVANCED = false>
      bool CastsTo() const;
      template<CT::NotVoid, bool BINARY_COMPATIBLE = false>
      bool CastsTo(size_t) const;

      /// Dereference the first element inside the container                  
      constexpr auto& operator * (this auto&& self) has_assumptions
      requires (not TypeErased and requires { *self.template GetRawAs<TYPE>(); }) {
         LglsAssumeDev(not self.IsEmpty(), "Container is empty");
         return *self.template GetRawAs<TYPE>();
      }

      /// Access the first element inside the container                       
      constexpr auto* operator -> (this auto&& self) has_assumptions
      requires (not TypeErased and requires { self.template GetRawAs<TYPE>(); }) {
         LglsAssumeDev(not self.IsEmpty(), "Container is empty");
         return self.template GetRawAs<TYPE>();
      }
      
      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      ///   @tparam T - the new type                                          
      template<CT::NotVoid T, CT::Container C>
      void SetType(this C& self) {
         static_assert(CT::NotSheddable<T>, "Strip all sheddables first");
         static_assert(CT::NotReference<T>, "Strip all references first");
         
         const auto type = MetaDataOf<T>();
         if constexpr (C::TypeErased)
            self.SetType(type);
         else {
            static_assert(Exact<T, TYPE>, "Type mismatch");         
            self.SetTypeInner(type);
         }
      }

      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      /// This particular override doesn't benefit from compile-time checks.  
      ///   @param type - the new type                                        
      template<CT::Container C>
      void SetType(this C& self, META type) {
         auto& t = self.GetTypeInner();
         
         if constexpr (CT::TypeErased<C>) {
            // This container is type-erased                            
            if (t == type)
               return;
         
            if (not t) {
               t = type;
               return;
            }

            LglsAssert(not self.IsTypeConstrained(),
               "Attempting to mutate type-locked container"
               " of type ", t, " to type ", type
            );

            /*if (t.CastsTo(type)) {
               // Type is compatible, but only sparse data can mutate   
               // freely. Dense containers can't mutate because their   
               // destructors might be wrong later                      
               LglsAssert(t.IsSparse(), "Can't mutate ", t,
                  " to incompatible type ", type);
            }
            else {*/
               // Type is not compatible, but container is not typed, so
               // if it has no constructed elements we can still mutate 
               LglsAssert(self.IsEmpty(), "Can't mutate ", t,
                  " to incompatible type ", type);
            /*}*/
            
            t = type;
         }
         else {
            // This container is statically typed                       
            if (not t)
               t = MetaDataOf<TYPE>();
            LglsAssert(t.IsExact(type), "Type mismatch");
         }
      }
   };
}
