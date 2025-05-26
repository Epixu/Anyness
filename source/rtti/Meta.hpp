#pragma once
#include <Langulus/Core.hpp>


namespace Langulus::RTTI::Inner
{

   ///                                                                        
   ///   Meta ID                                                              
   ///                                                                        
   /// Can be a naked pointer to a verb/trait/data/constant definition, or    
   /// a structured ID to one, that is either packed to a smaller size, or    
   /// carries a lot of meta information in the ID itself to avoid            
   /// indirections - all this is configurable                                
   ///                                                                        

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   /// Relies on the definition limits to pack an ID into the smallest        
   /// possible space. We would never have 64bit worth of type definitions    
   /// in a program either way. If somehow you do, then you're probably doing 
   /// something wrong. The handle has to be transformed into a pointer, so   
   /// this requires an additional level of indirection                       
   ///   @tparam BYTESIZE - the size of the handle in bytes                   
   template<unsigned BYTESIZE>
   struct MetaPacked {
      uint8_t mHandle[BYTESIZE];
   };
#endif

   /// A naked pointer to a definition. Probably the fastest, but most        
   /// memory-inefficient on 64bit systems                                    
   template<class T>
   struct MetaNaked {
   protected:
      const T* mDefinition = nullptr;

   public:
      constexpr MetaNaked() noexcept = default;
      constexpr MetaNaked(const MetaNaked&) noexcept = default;
      constexpr MetaNaked(MetaNaked&&) noexcept = default;
      constexpr MetaNaked(::std::nullptr_t) noexcept {}

      constexpr explicit MetaNaked(const T* definition) noexcept
         : mDefinition {definition} {}

      constexpr MetaNaked& operator = (const MetaNaked&) noexcept = default;
      constexpr MetaNaked& operator = (MetaNaked&&) noexcept = default;
      constexpr MetaNaked& operator = (::std::nullptr_t) noexcept {
         mDefinition = nullptr;
         return *this;
      }
      constexpr MetaNaked& operator = (const T* definition) noexcept {
         mDefinition = definition;
         return *this;
      }

      constexpr explicit operator bool() const noexcept {
         return mDefinition != nullptr;
      }

      constexpr bool IsExact(const MetaNaked& rhs) const noexcept {
         return mDefinition == rhs.mDefinition;
      }

      constexpr bool operator == (const MetaNaked& rhs) const noexcept {
         return mDefinition == rhs.mDefinition;
      }
   };

} // namespace Langulus::RTTI::Inner