assert_spec linux

proc cbe_image_file { } {
	return "cbe_block.img"
}

append build_components {
	core init timer
	server/ram_block
	server/cbe
	server/cbe_block
	app/block_tester
	server/log_terminal
	server/report_rom

	server/lx_block
	app/block_tester
}

build $build_components

create_boot_directory

append config {
<config verbose="yes">
	<parent-provides>
		<service name="ROM"/>
		<service name="RAM"/>
		<service name="IRQ"/>
		<service name="IO_MEM"/>
		<service name="IO_PORT"/>
		<service name="PD"/>
		<service name="RM"/>
		<service name="CPU"/>
		<service name="LOG"/>
	</parent-provides>

	<default-route>
		<any-service> <parent/> <any-child/> </any-service>
	</default-route>

	<default caps="100"/>
	<start name="timer">
		<resource name="RAM" quantum="1M"/>
		<provides><service name="Timer"/></provides>
	</start>

	<start name="report_rom">
		<resource name="RAM" quantum="32M"/>
		<provides> <service name="Report"/> <service name="ROM"/> </provides>
		<config verbose="yes"/>
	</start>

	<start name="block_device" ld="no">
		<binary name="lx_block"/>
		<resource name="RAM" quantum="2M"/>
		<provides> <service name="Block"/> </provides>
		<config file="} [cbe_image_file] {" block_size="4K" writeable="yes"/>
	</start>

	<start name="cbe">
		<resource name="RAM" quantum="4M"/>
		<provides><service name="Block"/></provides>
		<config show_progress="no" show_if_progress="no" sync_interval="1"/>
		<route>
			<service name="Block"> <child name="block_device"/> </service>
			<service name="Terminal"> <child name="log_terminal"/> </service>
			<service name="Timer"> <child name="timer"/> </service>
			<any-service> <parent/> </any-service>
		</route>
	</start>

	<start name="block_tester" caps="200">
		<resource name="RAM" quantum="16M"/>
		<config verbose="yes" report="no" log="yes" stop_on_error="yes">}
append config $block_tester_config

append config {
		</config>
		<route>
			<service name="Block"> <child name="cbe"/> </service>
			<any-service> <parent/> <any-child/> </any-service>
		</route>
	</start>
</config>}

install_config $config

append boot_modules {
	core init timer cbe
	ld.lib.so spark.lib.so libsparkcrypto.lib.so
	report_rom lx_block
	block_tester
}

append boot_modules [cbe_image_file]

build_boot_image $boot_modules

append qemu_args " -nographic -m 1536 "

run_genode_until {.*child "block_tester" exited with exit value 0.*\n} 240
