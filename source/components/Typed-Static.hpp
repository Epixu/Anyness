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
#include <Langulus/CT/Deep.hpp>


namespace Langulus::Anyness
{
   using DMeta = RTTI::DMeta;
   using TMeta = RTTI::TMeta;
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Defines the contained type at compile-time.                            
   /// Doesn't allow for type-erasure and doesn't take up space.              
   ///   @tparam META - the type of the definition                            
   ///   @tparam TYPE - static type, can't be void                            
   ///   @tparam ID   - which heap/stack is typed?                            
   template<class META, CT::NotVoid TYPE, unsigned ID>
   struct TypedStatic {
      using CTTI_Component = Yes<>;
      using CTTI_Typed     = TYPE;
      using CTTI_Sparse    = Maybe<CT::Sparse<TYPE>>;
      static constexpr int ComponentPrecedence = -3000;

      static constexpr bool TypeErased = false;
      static constexpr bool Sparse     = CT::Sparse<TYPE>;
      static constexpr bool Dense      = CT::Dense<TYPE>;

      /// Get the reflected type definition                                   
      META GetType() const noexcept { return MetaOf<TYPE>(); }

      /// Get the size of a single element of TYPE in bytes                   
      constexpr size_t GetStride() const noexcept { return sizeof(TYPE); }

      /// Get the reflected type name                                         
      constexpr auto GetName() const noexcept { return NameOf<TYPE>(); }

      /// Statically typed containers are always typed                        
      constexpr bool IsTyped() const noexcept { return true;  }

      /// Check if type origin is the same as one of the provided types       
      /// Always happens at compile-time                                      
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @tparam A1, AN... - the types to compare against                  
      ///   @return true if origin type is same to at least one of the types  
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool Is() const noexcept {
         return CT::SameAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if type origin is the same as another                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param type - the type to check for                               
      ///   @return true if this container has similar data                   
      bool Is(META type) const noexcept {
         return GetType().Is(type);
      }
      
      /// Check if type origin is the same as another container's type        
      /// This can potentially happen at compile-time                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param other - the type to check for                              
      ///   @return true if this container has similar data                   
      template<CT::Container C>
      constexpr bool Is(C const& other) const noexcept {
         if constexpr (CT::Untyped<C>)
            return GetType().Is(other.GetType());
         else
            return CT::Same<TYPE, TypeOf<C>>;
      }

      /// Check if unqualified type is the same as one of the provided types  
      /// Always happens at compile-time                                      
      ///   @attention ignores only cv-qualifiers                             
      ///   @tparam A1, AN... - the types to compare against                  
      ///   @return true if data type is similar to at least one of the types 
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool IsSimilar() const noexcept {
          return CT::SimilarAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if unqualified type is the same as another                    
      ///   @attention ignores only cv-qualifiers                             
      ///   @param type - the type to check for                               
      ///   @return true if this block contains similar data                  
      bool IsSimilar(META type) const noexcept {
         return GetType().IsSimilar(type);
      }

      /// Check if unqualified type is the same as another container's type   
      /// This can potentially happen at compile-time                         
      ///   @attention ignores only cv-qualifiers                             
      ///   @param other - the container to check for                         
      ///   @return true if this container has similar data                   
      template<CT::Container C>
      constexpr bool IsSimilar(C const& other) const noexcept {
         if constexpr (CT::Untyped<C>)
            return GetType().IsSimilar(other.GetType());
         else
            return CT::Similar<TYPE, TypeOf<C>>;
      }

      /// Check if this type is exactly one of the provided types             
      /// Always happens at compile-time                                      
      ///   @tparam T1, TN... - the types to compare against                  
      ///   @return true if data type matches at least one type               
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool IsExact() const noexcept {
         return CT::ExactAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if this type is exactly another                               
      ///   @param type - the type to match                                   
      ///   @return true if data type matches type exactly                    
      bool IsExact(META type) const noexcept {
         return GetType().IsExact(type);
      }

      /// Check if this type is exactly another container's type              
      /// This can potentially happen at compile-time                         
      ///   @param other - the block to match                                 
      ///   @return true if data type matches type exactly                    
      template<CT::Container C>
      constexpr bool IsExact(C const& other) const noexcept {
         if constexpr (CT::Untyped<C>)
            return GetType().IsExact(other.GetType());
         else
            return CT::Exact<TYPE, TypeOf<C>>;
      }

      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      constexpr bool IsSparse() const noexcept {
         return CT::Sparse<TYPE>;
      }
      
      /// Check if contained data is constant                                 
      ///   @return true if the contents are constant                         
      constexpr bool IsConstant(this auto const& self) noexcept {
         return CT::Constant<TYPE> or not self.GetAllocation();
      }

      /// Check if container is made of other containers                      
      ///   @return true if the container is deep                             
      constexpr bool IsDeep() const noexcept {
         return CT::Deep<Decay<TYPE>>;
      }

      /// Get the size of the type times the contained elements               
      ///   @return the size of all elements in bytes                         
      template<CT::Container C>
      constexpr bool GetBytesize(this C const& self) noexcept {
         return sizeof(TYPE) * self.GetCount();
      }

      /// Dereference the first element inside the container                  
      constexpr TYPE& operator * (this auto&& self) has_assumptions
      requires requires { *self.template GetRawAs<TYPE>(); } {
         LglsAssumeDev(not self.IsEmpty(), "Container is empty");
         return *self.template GetRawAs<TYPE>();
      }

      /// Access the first element inside the container                       
      constexpr TYPE* operator -> (this auto&& self) has_assumptions
      requires requires { self.template GetRawAs<TYPE>(); } {
         LglsAssumeDev(not self.IsEmpty(), "Container is empty");
         return self.template GetRawAs<TYPE>();
      }
      
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners                            
      ///   @tparam T - the new type                                          
      template<CT::NotVoid T>
      consteval void SetType() {
         static_assert(CT::Exact<T, TYPE>, "Type mismatch");
      }

      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners                            
      /// This particular override doesn't benefit from compile-time checks   
      ///   @param type - the new type                                        
      void SetType(META type) {
         LglsAssert(type.IsExact(MetaDataOf<TYPE>()), "Type mismatch");
      }
   };
}
