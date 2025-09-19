///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Same.hpp>
#include <Langulus/MetaOf.hpp>
#include <Langulus/CT/Deep.hpp>


namespace Langulus::Anyness
{
   using DMeta = RTTI::DMeta;
   using TMeta = RTTI::TMeta;
}

namespace Langulus::Anyness::Component
{
   template<unsigned>
   struct IterationForEach;

   template<unsigned>
   struct HeapMovable;


   ///                                                                        
   /// Defines the contained type as a member variable, allowing the use of   
   /// type-erasure. You can optionally constrain the type                    
   ///   @tparam META - the type of the meta                                  
   ///   @tparam TYPE - optionally static type, use void for type-erasure     
   ///   @tparam ID   - which heap/stack is typed?                            
   template<class META, class TYPE, unsigned ID>
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
      /*template<unsigned>
      friend struct IterationForEach;*/
      template<unsigned>
      friend struct Removal;
      template<unsigned>
      friend struct HeapMovable;

      /// Reset the type of the container, unless it's type-constrained.      
      /// If this container isn't type-erased, this call is a no-op.          
      constexpr void ResetType(this auto& self) noexcept {
         if constexpr (TypeErased) {
            if constexpr (requires { self.IsTypeConstrained(); }) {
               if (not self.IsTypeConstrained())
                  self.mType = {};
            }
            else self.mType = {};
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
      
      /// Default-initialize the component                                    
      /*constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetTypeInner({});
      }*/

   public:
      /// Get the contained type                                              
      constexpr META GetType(this auto const& self) noexcept {
         META const& meta = self.GetTypeInner();
         if constexpr (not TypeErased)
            const_cast<META&>(meta) = MetaDataOf<TYPE>();
         return meta;
      }

      /// Get the size of a single element in bytes                           
      constexpr size_t GetStride(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().GetSize();
         else
            return sizeof(TYPE);
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
      /// This can potentially happen at compile-time                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @tparam A1, AN... - the types to compare against                  
      ///   @return true if origin type is same to at least one of the types  
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool Is(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().template Is<A1, AN...>();
         else
            return CT::SameAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if type origin is the same as another                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param type - the type to check for                               
      ///   @return true if this container has similar data                   
      bool Is(this auto const& self, META type) noexcept {
         return self.GetTypeInner().Is(type);
      }

      /// Check if type origin is the same as another container's type        
      /// This can potentially happen at compile-time                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param other - the type to check for                              
      ///   @return true if this container has similar data                   
      template<CT::Container C>
      constexpr bool Is(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return self.GetTypeInner().Is(other.mType);
         else
            return CT::Same<TYPE, TypeOf<C>>;
      }

      /// Check if unqualified type is the same as one of the provided types  
      /// This can potentially happen at compile-time                         
      ///   @attention ignores only cv-qualifiers                             
      ///   @tparam A1, AN... - the types to compare against                  
      ///   @return true if data type is similar to at least one of the types 
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool IsSimilar(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().template IsSimilar<A1, AN...>();
         else
            return CT::SimilarAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if unqualified type is the same as another                    
      ///   @attention ignores only cv-qualifiers                             
      ///   @param type - the type to check for                               
      ///   @return true if this block contains similar data                  
      bool IsSimilar(this auto const& self, META type) noexcept {
         return self.GetTypeInner().IsSimilar(type);
      }

      /// Check if unqualified type is the same as another container's type   
      /// This can potentially happen at compile-time                         
      ///   @attention ignores only cv-qualifiers                             
      ///   @param other - the container to check for                         
      ///   @return true if this container has similar data                   
      template<CT::Container C>
      constexpr bool IsSimilar(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return self.GetTypeInner().IsSimilar(other.mType);
         else
            return CT::Similar<TYPE, TypeOf<C>>;
      }

      /// Check if this type is exactly one of the provided types             
      /// This can potentially happen at compile-time                         
      ///   @tparam T1, TN... - the types to compare against                  
      ///   @return true if data type matches at least one type               
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool IsExact(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().template IsExact<A1, AN...>();
         else
            return CT::ExactAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if this type is exactly another                               
      ///   @param type - the type to match                                   
      ///   @return true if data type matches type exactly                    
      bool IsExact(this auto const& self, META type) noexcept {
         return self.GetTypeInner().IsExact(type);
      }

      /// Check if this type is exactly another container's type              
      /// This can potentially happen at compile-time                         
      ///   @param other - the block to match                                 
      ///   @return true if data type matches type exactly                    
      template<CT::Container C>
      constexpr bool IsExact(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return self.GetTypeInner().IsExact(other.mType);
         else
            return CT::Exact<TYPE, TypeOf<C>>;
      }
      
      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      constexpr bool IsSparse(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetTypeInner().IsSparse();
         else
            return CT::Sparse<TYPE>;
      }
      
      /// Check if block is constant                                          
      ///   @attention disowned containers are always constant                
      ///   @return true if the contents are constant                         
      constexpr bool IsConstant(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return not self.GetAllocation() or self.mType.IsConstant();
         else
            return CT::Constant<TYPE> or not self.GetAllocation();
      }

      /// Check if container is made of other containers                      
      ///   @return true if the container is deep                             
      constexpr bool IsDeep(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.GetAllocation().IsDeep();
         else
            return CT::Deep<Decay<TYPE>>;
      }

      /// Returns true if a type constraint is specified                      
      constexpr bool IsTypeConstrained() const requires (not TypeErased) {
         return true;
      }

      /// Get the size of the type times the contained elements               
      ///   @return the size of all elements in bytes                         
      constexpr bool GetBytesize(this auto const& self) noexcept {
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
      
      /// Set the contained data type if possible                             
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners                            
      ///   @tparam T - the new type                                          
      template<CT::NotVoid T, CT::Container C>
      void SetType(this C& self) {
         const auto type = MetaDataOf<T>();
         if constexpr (C::TypeErased)
            self.SetType(type);
         else {
            static_assert(CT::Exact<T, TYPE>, "Type mismatch");         
            self.SetTypeInner(type);
         }
      }

      /// Set the contained data type if possible                             
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners                            
      /// This particular override doesn't benefit from compile-time checks   
      ///   @param type - the new type                                        
      template<CT::Container C>
      void SetType(this C& self, META type) {
         META& mType = const_cast<META&>(self.GetTypeInner());
         
         if constexpr (C::TypeErased) {
            // This container is type-erased                            
            if (mType == type)
               return;
         
            if (not mType) {
               mType = type;
               return;
            }

            LglsAssert(not self.IsTypeConstrained(),
               "Attempting to mutate type-locked container"
               " of type ", mType, " to type ", type
            );

            if (mType->CastsTo(type)) {
               // Type is compatible, but only sparse data can mutate   
               // freely. Dense containers can't mutate because their   
               // destructors might be wrong later                      
               LglsAssert(self.IsSparse(), "Can't mutate ", mType,
                  " to incompatible type ", type);
            }
            else {
               // Type is not compatible, but container is not typed, so
               // if it has no constructed elements we can still mutate 
               LglsAssert(self.IsEmpty(), "Can't mutate ", mType,
                  " to incompatible type ", type);
            }
            
            mType = type;
         }
         else {
            // This container is statically typed                       
            if (not mType)
               mType = MetaDataOf<TYPE>();
            LglsAssert(mType.IsExact(type), "Type mismatch");
         }
      }

      /// Make container type constant                                        
      ///   @attention this will throw an exception if constant type hasn't   
      ///      been reflected yet                                             
      /*void MakeConstant() {
         mType = mType.AddConst();
      }
      
      /// Remove the topmost type constness                                   
      void MakeMutableOnce() noexcept {
         mType = mType.GetDecvq();
      }

      /// Remove all qualifier from all levels of indirection                 
      void MakeMutableAll() noexcept {
         mType = mType.GetDecvqAll();
      }*/
   };
}
