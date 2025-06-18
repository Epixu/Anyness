///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Core.hpp>


namespace Langulus::RTTI::Inner
{

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   ///                                                                        
   /// Relies on the definition limits to pack an ID into the smallest        
   /// possible space. We would never have 64bit worth of type definitions    
   /// in a program either way. If somehow you do, then you're probably doing 
   /// something wrong. The handle has to be transformed into a pointer, so   
   /// this requires an additional level of indirection                       
   ///   @tparam T - the type of the meta (data/tag/verb/const)               
   ///   @tparam BYTESIZE - the size of the handle in bytes                   
   ///                                                                        
   #pragma pack(push, 1)
   template<class T, unsigned BYTESIZE>
   struct MetaPacked {
   protected:
      static constexpr uint8_t Zero[BYTESIZE] {0};
      uint8_t mHandle[BYTESIZE] {0};

   public:
      constexpr MetaPacked() noexcept = default;
      constexpr MetaPacked(const MetaPacked&) noexcept = default;
      constexpr MetaPacked(MetaPacked&&) noexcept = default;
      constexpr MetaPacked(size_t id) noexcept {
         static_assert(sizeof(size_t) >= BYTESIZE);
         memcpy(mHandle, &id, BYTESIZE);
      }

      constexpr MetaPacked& operator = (const MetaPacked&) noexcept = default;
      constexpr MetaPacked& operator = (MetaPacked&&) noexcept = default;
      constexpr MetaPacked& operator = (size_t id) noexcept {
         static_assert(sizeof(size_t) >= BYTESIZE);
         memcpy(mHandle, &id, BYTESIZE);
         return *this;
      }

      constexpr explicit operator bool() const noexcept {
         return 0 != memcmp(mHandle, Zero, BYTESIZE);
      }

      constexpr bool operator == (const MetaPacked& rhs) const noexcept {
         return 0 == memcmp(mHandle, rhs.mHandle, BYTESIZE);
      }
   };
   #pragma pack(pop)
#endif


   ///                                                                        
   /// A naked pointer to a definition. Probably the fastest, but most        
   /// memory-inefficient on 64bit systems                                    
   ///   @tparam T - the type of the meta (data/tag/verb/const)               
   ///                                                                        
   template<class T>
   struct MetaNaked {
   protected:
      const T* mDefinition = nullptr;

   public:
      constexpr MetaNaked() noexcept = default;
      constexpr MetaNaked(const MetaNaked&) noexcept = default;
      constexpr MetaNaked(MetaNaked&&) noexcept = default;
      constexpr MetaNaked(::std::nullptr_t) noexcept {}
      constexpr MetaNaked(const T* definition) noexcept
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