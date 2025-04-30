#pragma once
#include "Meta.hpp"
#include "Intent.hpp"


namespace Langulus::RTTI
{
   class DefinitionData;

   namespace Inner
   {
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      /// Relies on the definition limits to pack an ID into the smallest     
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      struct MetaDataStructured_8_8 : MetaPacked<1> {

      };

      struct MetaDataStructured_16_16 : MetaPacked<2> {

      };

      struct MetaDataStructured_24_8 : MetaPacked<3> {

      };

      struct MetaDataStructured_32_8 : MetaPacked<4> {

      };

      struct MetaDataStructured_32_16 : MetaPacked<4> {

      };
   #endif

      ///                                                                     
      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      ///                                                                     
      struct MetaDataNaked {
      private:
         const DefinitionData* mDefinition = nullptr;

      public:
         constexpr MetaDataNaked() noexcept = default;
         constexpr MetaDataNaked(const DefinitionData* definition) noexcept
            : mDefinition {definition} {}

         explicit operator bool() const noexcept {
            return mDefinition != nullptr;
         }

         template<class, class...>
         bool Is() const noexcept;
         bool Is(const MetaDataNaked&) const noexcept;

         template<class, class...>
         bool IsSimilar() const noexcept;
         bool IsSimilar(const MetaDataNaked&) const noexcept;

         template<class, class...>
         bool IsExact() const noexcept;
         bool IsExact(const MetaDataNaked&) const noexcept;

         /// Compare if two data types match exactly                          
         ///   @attention includes qualifiers and sparsity                    
         bool operator == (const MetaDataNaked& rhs) const noexcept {
            return IsExact(rhs);
         }

         ::std::size_t GetMinAllocation() const noexcept;

         bool IsDense() const noexcept;
         bool IsSparse() const noexcept;
         bool IsConstant() const noexcept;
         bool IsMutable() const noexcept;
         bool IsDeep() const noexcept;
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaDataBase = MetaDataStructured_16_16;
   #else
      using MetaDataBase = MetaDataNaked;
   #endif

   } // namespace Langulus::RTTI::Inner
   

   ///                                                                        
   ///   Data type ID                                                         
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection                                  
   ///                                                                        
   struct MetaData : Inner::MetaDataBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      constexpr MetaData() noexcept = default;
      constexpr MetaData(const MetaData&) noexcept = default;
      constexpr MetaData(MetaData&&) noexcept = default;

      constexpr MetaData(::std::nullptr_t) noexcept {}
      constexpr MetaData(const DefinitionData* definition) noexcept
         : Inner::MetaDataBase {definition} {}
      constexpr MetaData(Cloned<MetaData>&& meta) noexcept
         : MetaData {*meta} {}
   };

   using DMeta = MetaData;

} // namespace Langulus::RTTI