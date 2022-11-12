# frozen_string_literal: true

module RBS
  module Collection

    # This class represent the configration file.
    class Config
      class LockfileGenerator
        attr_reader :config, :bundler_definition, :lockfile, :existing_lockfile

        def self.generate(config_path:, gemfile_lock_path:, with_lockfile: true)
          gemfile_path = gemfile_lock_path.sub_ext("")
          definition = Bundler::Definition.build(gemfile_path, gemfile_lock_path, {})

          generator = new(config_path: config_path, bundler_definition: definition, with_lockfile: with_lockfile)
          generator.generate

          [
            generator.config,
            generator.lockfile
          ]
        end

        def initialize(config_path:, bundler_definition:, with_lockfile:)
          @config = Config.from_path config_path
          @bundler_definition = bundler_definition

          lock_path = Config.to_lockfile_path(config_path)

          @lockfile = Lockfile.new(file_path: lock_path, path: Pathname(config.data_path))
          config.sources.each do |source|
            case source
            when Sources::Git
              lockfile.sources << source
            end
          end

          if with_lockfile && lock_path.file?
            @existing_lockfile = Lockfile.load(lock_path, YAML.load_file(lock_path.to_s))
          end

          @gem_map = bundler_definition.locked_gems.specs.each.with_object({}) do |spec, hash|
            hash[spec.name] = spec
          end
        end

        def generate
          ignored_gems = config.gems.select {|gem| gem['ignore'] }.map {|gem| gem['name'] }.to_set

          config.gems.each do |gem|
            assign_gem(name: gem['name'], version: gem['version'], ignored_gems: ignored_gems)
          end

          bundler_definition.requires.each do |gem, requires|
            unless requires.empty?
              spec = @gem_map[gem] or raise
              assign_gem(name: spec.name, version: spec.version, ignored_gems: ignored_gems)
            end
          end

          content = YAML.dump(lockfile.dump)
          lockfile.file_path.write(content)
        end

        private def assign_gem(name:, version:, ignored_gems:)
          return if lockfile.gems.key?(name)

          begin
            # @type var locked: Lockfile::gem?
            if existing_lockfile
              locked = existing_lockfile.gems[name]
            end

            # If rbs_collection.lock.yaml contain the gem, use it.
            # Else find the gem from gem_collection.
            unless locked
              source = find_source(name: name)
              return unless source

              installed_version = version
              best_version = find_best_version(version: installed_version, versions: source.versions(name))

              locked = {
                name: name,
                version: best_version.to_s,
                source: source,
              }
            end

            locked or raise

            unless ignored_gems.include?(name)
              lockfile.gems[locked[:name]] = locked

              source = locked[:source]
              source.dependencies_of(locked[:name], locked[:version])&.each do |dep|
                assign_stdlib(name: dep.name)
              end
            end
          ensure
            each_dependent_gem(name: name) do |spec|
              assign_gem(name: spec.name, version: spec.version, ignored_gems: ignored_gems)
            end
          end
        end

        private def assign_stdlib(name:)
          return if lockfile.gems.key?(name)

          source = Sources::Stdlib.instance
          raise "Cannot find stdlib RBS of `#{name}`" unless source.has?(name, nil)

          version = source.versions(name).last || raise
          lockfile.gems[name] = { name: name, version: version, source: source }
          if deps = source.dependencies_of(name, version)
            deps.each do |dep|
              assign_stdlib(name: dep.name)
            end
          end
        end

        private def each_dependent_gem(name:)
          if spec = @gem_map[name]
            spec.dependencies.each do |dep|
              yield @gem_map[dep.name]
            end
          end
        end

        private def find_source(name:)
          sources = config.sources
          _ = sources.find { |c| c.has?(name, nil) }
        end

        private def find_best_version(version:, versions:)
          candidates = versions.map { |v| Gem::Version.create(v) or raise }
          return candidates.max || raise unless version

          v = Gem::Version.create(version) or raise
          Repository.find_best_version(v, candidates)
        end
      end
    end
  end
end
