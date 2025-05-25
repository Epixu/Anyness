#pragma once
#include <Langulus/MetaOf.hpp>
#include <Langulus/CT/Deep.hpp>


namespace Langulus::Anyness
{

   using DMeta = RTTI::DMeta;
   using TMeta = RTTI::TMeta;

} // namespace Langulus::Anyness

namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Defines the contained type at compile-time                             
   /// Doesn't allow for type-erasure and doesn't take up space               
   ///   @tparam T    - the type of the variable                              
   ///   @tparam TYPE - static type, can't be void                            
   ///   @tparam ID   - which heap/stack is typed?                            
   template<class T, CT::NotVoid TYPE, unsigned ID = 0>
   struct TypedStatic {
      using CTTI_Component = Yes;
      using CTTI_Typed     = TYPE;
      using CTTI_Sparse    = Maybe<CT::Sparse<TYPE>>;

      static constexpr bool TypeErased = false;
      static constexpr bool Sparse     = CT::Sparse<TYPE>;
      static constexpr bool Dense      = CT::Dense<TYPE>;

      constexpr TypedStatic() noexcept = default;
      explicit constexpr TypedStatic(const TypedStatic&) noexcept = default;
      explicit constexpr TypedStatic(TypedStatic&&) noexcept = default;
      template<template<class> class I> requires CT::Intent<I<TypedStatic>>
      constexpr TypedStatic(I<TypedStatic>&&) noexcept {}

      constexpr TypedStatic& operator = (TypedStatic const&) noexcept = default;
      constexpr TypedStatic& operator = (TypedStatic&&) noexcept = default;
      template<template<class> class I> requires CT::Intent<I<TypedStatic>>
      constexpr TypedStatic& operator = (I<TypedStatic>&&) {}

      /// Get the reflected type definition                                   
      ///   @return the definition                                            
      T GetType() const noexcept { return MetaOf<TYPE>(); }

      /// Get the reflected type name                                         
      constexpr auto GetName()   const noexcept { return NameOf<TYPE>(); }

      /// Statically typed containers are always typed                        
      constexpr bool IsTyped()   const noexcept { return true;  }
      constexpr bool IsUntyped() const noexcept { return false; }

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
      bool Is(T type) const noexcept {
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
      bool IsSimilar(T type) const noexcept {
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
      bool IsExact(T type) const noexcept {
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
      
      /// Check if container contains dense data                              
      ///   @returns true if this container refers to dense memory            
      constexpr bool IsDense() const noexcept {
         return CT::Dense<TYPE>;
      }

      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      constexpr bool IsSparse() const noexcept {
         return CT::Sparse<TYPE>;
      }
      
      /// Check if contained data is constant                                 
      ///   @return true if the contents are constant                         
      constexpr bool IsConstant() const noexcept {
         return CT::Constant<TYPE>;
      }

      /// Check if constained data is mutable                                 
      ///   @return true if the contents are mutable                          
      constexpr bool IsMutable() const noexcept {
         return CT::Mutable<TYPE>;
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

      /// Dereference the first pointer inside the container, if sparse       
      constexpr TYPE& operator * (this auto&& self) has_assumptions {
         AssumeDev(not self.IsEmpty(), HERE(), "Container is empty");
         return self.template GetInner<ID, TYPE>();
      }

      /// Get the first pointer inside the container, if sparse               
      constexpr TYPE& operator -> (this auto&& self) has_assumptions {
         AssumeDev(not self.IsEmpty(), HERE(), "Container is empty");
         return self.template GetInner<ID, TYPE>();
      }
   };

} // namespace Langulus::Anyness::Component
