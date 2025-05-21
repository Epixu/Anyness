#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Same.hpp>
#include <Langulus/MetaOf.hpp>
#include <Langulus/CT/Deep.hpp>


namespace Langulus::Anyness
{

   using DMeta = RTTI::DMeta;
   using TMeta = RTTI::TMeta;

} // namespace Langulus::Anyness

namespace Langulus::Anyness::Component
{

   template<unsigned>
   struct IterationForEach;

   template<unsigned>
   struct HeapMovable;


   ///                                                                        
   /// Defines the contained type as a member variable, allowing the use of   
   /// type-erasure. You can optionally constrain the type                    
   ///   @tparam T    - the type of the variable                              
   ///   @tparam TYPE - optionally static type, use void for type-erasure     
   ///   @tparam ID   - which heap/stack is typed?                            
   template<class T, class TYPE = void, unsigned ID = 0>
   struct TypedStack {
      using CTTI_Component = Yes;
      using CTTI_Typed     = TYPE;

      static constexpr bool TypeErased = CT::Void<TYPE>;
      /// @attention valid only if not TypeErased                       
      static constexpr bool Sparse = not TypeErased and CT::Sparse<TYPE>;
      /// @attention valid only if not TypeErased                       
      static constexpr bool Dense = not TypeErased and CT::Dense<TYPE>;

   protected:
      template<unsigned>
      friend struct IterationForEach;
      template<unsigned>
      friend struct HeapMovable;

      // The type                                                       
      T mType;

   public:
      /// Get the contained type                                              
      ///   @return the contained type                                        
      constexpr T GetType() const noexcept { return mType; }

      /// Check if block has a data type                                      
      ///   @return true if data contained in this pack is specified          
      constexpr bool IsTyped() const noexcept {
         if constexpr (TypeErased)
            return static_cast<bool>(mType);
         else
            return true;
      }

      /// Check if block has a data type                                      
      ///   @return true if data contained in this pack is unspecified        
      constexpr bool IsUntyped() const noexcept {
         if constexpr (TypeErased)
            return not static_cast<bool>(mType);
         else
            return false;
      }

      /// Check if type origin is the same as one of the provided types       
      /// This can potentially happen at compile-time                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @tparam A1, AN... - the types to compare against                  
      ///   @return true if origin type is same to at least one of the types  
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool Is() const noexcept {
         if constexpr (TypeErased)
            return mType.template Is<A1, AN...>();
         else
            return CT::SameAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if type origin is the same as another                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param type - the type to check for                               
      ///   @return true if this container has similar data                   
      bool Is(T type) const noexcept {
         return mType.Is(type);
      }

      /// Check if type origin is the same as another container's type        
      /// This can potentially happen at compile-time                         
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param other - the type to check for                              
      ///   @return true if this container has similar data                   
      template<CT::Container C>
      constexpr bool Is(C const& other) const noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return mType.Is(other.mType);
         else
            return CT::Same<TYPE, TypeOf<C>>;
      }

      /// Check if unqualified type is the same as one of the provided types  
      /// This can potentially happen at compile-time                         
      ///   @attention ignores only cv-qualifiers                             
      ///   @tparam A1, AN... - the types to compare against                  
      ///   @return true if data type is similar to at least one of the types 
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool IsSimilar() const noexcept {
         if constexpr (TypeErased)
            return mType.template IsSimilar<A1, AN...>();
         else
            return CT::SimilarAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if unqualified type is the same as another                    
      ///   @attention ignores only cv-qualifiers                             
      ///   @param type - the type to check for                               
      ///   @return true if this block contains similar data                  
      bool IsSimilar(T type) const noexcept {
         return mType.IsSimilar(type);
      }

      /// Check if unqualified type is the same as another container's type   
      /// This can potentially happen at compile-time                         
      ///   @attention ignores only cv-qualifiers                             
      ///   @param other - the container to check for                         
      ///   @return true if this container has similar data                   
      template<CT::Container C>
      constexpr bool IsSimilar(C const& other) const noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return mType.IsSimilar(other.mType);
         else
            return CT::Similar<TYPE, TypeOf<C>>;
      }

      /// Check if this type is exactly one of the provided types             
      /// This can potentially happen at compile-time                         
      ///   @tparam T1, TN... - the types to compare against                  
      ///   @return true if data type matches at least one type               
      template<CT::NotVoid A1, CT::NotVoid...AN>
      constexpr bool IsExact() const noexcept {
         if constexpr (TypeErased)
            return mType.template IsExact<A1, AN...>();
         else
            return CT::ExactAsOneOf<TYPE, A1, AN...>;
      }

      /// Check if this type is exactly another                               
      ///   @param type - the type to match                                   
      ///   @return true if data type matches type exactly                    
      bool IsExact(T type) const noexcept {
         return mType.IsExact(type);
      }

      /// Check if this type is exactly another container's type              
      /// This can potentially happen at compile-time                         
      ///   @param other - the block to match                                 
      ///   @return true if data type matches type exactly                    
      template<CT::Container C>
      constexpr bool IsExact(C const& other) const noexcept {
         if constexpr (TypeErased or C::TypeErased)
            return mType.IsExact(other.mType);
         else
            return CT::Exact<TYPE, TypeOf<C>>;
      }
      
      /// Check if container contains dense data                              
      ///   @returns true if this container refers to dense memory            
      constexpr bool IsDense() const noexcept {
         if constexpr (TypeErased)
            return mType.IsDense();
         else
            return CT::Dense<TYPE>;
      }

      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      constexpr bool IsSparse() const noexcept {
         if constexpr (TypeErased)
            return mType.IsSparse();
         else
            return CT::Sparse<TYPE>;
      }
      
      /// Check if block is constant                                          
      ///   @return true if the contents are constant                         
      constexpr bool IsConstant() const noexcept {
         if constexpr (TypeErased)
            return mType.IsConstant();
         else
            return CT::Constant<TYPE>;
      }

      /// Check if block is mutable                                           
      ///   @return true if the contents are mutable                          
      constexpr bool IsMutable() const noexcept {
         if constexpr (TypeErased)
            return mType.IsMutable();
         else
            return CT::Mutable<TYPE>;
      }

      /// Check if container is made of other containers                      
      ///   @return true if the container is deep                             
      constexpr bool IsDeep() const noexcept {
         if constexpr (TypeErased)
            return mType.IsDeep();
         else
            return CT::Deep<Decay<TYPE>>;
      }
      
      /// Get the size of the type                                            
      ///   @return the size of a single element in bytes                     
      constexpr bool GetStride() const noexcept {
         if constexpr (TypeErased)
            return mType.GetStride();
         else
            return sizeof(TYPE);
      }

      template<bool BINARY_COMPATIBLE = false, bool ADVANCED = false>
      bool CastsToMeta(T) const;
      template<bool BINARY_COMPATIBLE = false>
      bool CastsToMeta(T, ::std::size_t) const;

      template<CT::NotVoid, bool BINARY_COMPATIBLE = false, bool ADVANCED = false>
      bool CastsTo() const;
      template<CT::NotVoid, bool BINARY_COMPATIBLE = false>
      bool CastsTo(::std::size_t) const;

      template<CT::NotVoid>
      void SetType()  requires TypeErased;
      void SetType(T) requires TypeErased;
   };

} // namespace Langulus::Anyness::Component
