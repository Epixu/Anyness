///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "../BlockSet.hpp"


namespace Langulus::Anyness
{

   /// Checks type compatibility and sets type for the type-erased map        
   ///   @tparam T - the type                                                 
   template<CT::NotSemantic T> LANGULUS(INLINED)
   void BlockSet::Mutate() {
      Mutate(MetaDataOf<T>());
   }

   /// Checks type compatibility and sets type for the type-erased map        
   ///   @param key - the key type                                            
   LANGULUS(INLINED)
   void BlockSet::Mutate(DMeta key) {
      if (not mKeys.mType) {
         // Set a fresh key type                                        
         mKeys.mType = key;
      }
      else {
         // Key type already set, so check compatibility                
         LANGULUS_ASSERT(mKeys.IsExact(key), Mutate,
            "Attempting to mutate type-erased unordered map's key type"
         );
      }
   }

   /// Check if this value type is similar to one of the listed types,        
   /// ignoring density and cv-qualifiers                                     
   ///   @tparam T1, TN... - the types to compare against                     
   ///   @return true if value type is similar to at least one of the types   
   template<CT::Set THIS, CT::Data T1, CT::Data...TN> LANGULUS(INLINED)
   constexpr bool BlockSet::Is() const noexcept {
      return GetValues<THIS>().template Is<T1, TN...>();
   }
   
   /// Check if value type loosely matches a given type, ignoring             
   /// density and cv-qualifiers                                              
   ///   @param meta - the type to check for                                  
   ///   @return true if this set contains similar value data                 
   template<CT::Set THIS> LANGULUS(INLINED)
   bool BlockSet::Is(DMeta meta) const noexcept {
      return GetValues<THIS>().Is(meta);
   }

   /// Check if value type is similar to one of the listed types,             
   /// ignoring cv-qualifiers only                                            
   ///   @tparam T1, TN... - the types to compare against                     
   ///   @return true if value type is similar to at least one of the types   
   template<CT::Set THIS, CT::Data T1, CT::Data...TN> LANGULUS(INLINED)
   constexpr bool BlockSet::IsSimilar() const noexcept {
      return GetValues<THIS>().template IsSimilar<T1, TN...>();
   }
   
   /// Check if value type loosely matches a given type, ignoring             
   /// cv-qualifiers only                                                     
   ///   @param meta - the type to check for                                  
   ///   @return true if this set contains similar value data                 
   template<CT::Set THIS> LANGULUS(INLINED)
   bool BlockSet::IsSimilar(DMeta meta) const noexcept {
      return GetValues<THIS>().IsSimilar(meta);
   }

   /// Check if value type is exactly as one of the listed types,             
   /// including by density and cv-qualifiers                                 
   ///   @tparam T1, TN... - the types to compare against                     
   ///   @return true if value type exactly matches at least one type         
   template<CT::Set THIS, CT::Data T1, CT::Data...TN> LANGULUS(INLINED)
   constexpr bool BlockSet::IsExact() const noexcept {
      return GetValues<THIS>().template IsExact<T1, TN...>();
   }
   
   /// Check if value type is exactly the provided type,                      
   /// including the density and cv-qualifiers                                
   ///   @param type - the type to match                                      
   ///   @return true if this set contains this exact value data              
   template<CT::Set THIS> LANGULUS(INLINED)
   bool BlockSet::IsExact(DMeta meta) const noexcept {
      return GetValues<THIS>().IsExact(meta);
   }
   
   /// Check if types of two sets are compatible for writing                  
   ///   @param other - set to test with                                      
   ///   @return true if both sets are type-compatible                        
   template<CT::Set THIS> LANGULUS(INLINED)
   constexpr bool BlockSet::IsTypeCompatibleWith(CT::Set auto const& other) const noexcept {
      using RHS = Deref<decltype(other)>;
      if constexpr (CT::Typed<THIS, RHS>) {
         // Static type check                                           
         return CT::Similar<TypeOf<THIS>, TypeOf<RHS>>;
      }
      else {
         // Dynamic type check                                          
         return GetValues<THIS>().IsSimilar(other.GetType());
      }
   }

} // namespace Langulus::Anyness
